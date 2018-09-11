package carldata.borsuk.autoii

import java.time._
import java.time.temporal.ChronoUnit
import java.util.UUID
import java.util.UUID.randomUUID

import carldata.borsuk.autoii.ApiObjects.FitAutoIIParams
import carldata.borsuk.autoii.DryWeatherPattern._
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.series.Sessions.Session
import carldata.series.{Gen, TimeSeries}


case class RDIIObject(rainfall: TimeSeries[Double], flow: TimeSeries[Double], dwp: TimeSeries[Double]
                      , inflows: Seq[(String, TimeSeries[Double])])

class RDII(modelType: String) {
  val id: String = randomUUID().toString
  var model: Option[RDIIObject] = None
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitAutoIIParams): Unit = {

    if (params.flow.nonEmpty && params.flow.length == params.rainfall.length) {
      val features: Array[Array[Double]] = params.flow.indices.map(_ % 24).map(i => Array(i.toDouble)).toArray
      val endIndex: LocalDateTime = params.startDate.plusSeconds(params.resolution.getSeconds * params.flow.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(params.startDate), dtToInstant(endIndex), params.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(index.toVector, params.rainfall.toVector)
      val flow: TimeSeries[Double] = TimeSeries(index.toVector, params.flow.toVector)

      model = Some(RDIIBuilder(rainfall, flow, params.startDate, endIndex, params.window.toSeq)
        .withStormSessionWindows(params.stormSessionWindows)
        .withStormIntensityWindow(params.stormIntensityWindow)
        .withDryDayWindow(params.dryDayWindow)
        .withLimit(15)
        .build())

      buildNumber += 1
    }
  }

  /**
    * List all rdiis
    */
  def list(): Seq[(String, LocalDateTime, LocalDateTime)] = {
    model.map { x =>
      x.inflows
        .flatMap {
          t =>
            if (t._2.nonEmpty) Some(t._1, instantToLDT(t._2.index.head), instantToLDT(t._2.index.last))
            else None
        }
    }
      .getOrElse(Seq())
  }

  /**
    * For provided rdii id
    * return series of:
    * rainfall, flow, dwp and rdii
    */
  def get(rdii_id: String): Option[(LocalDateTime, LocalDateTime
    , Array[Double], Array[Double], Array[Double], Array[Double])] = {

    model.flatMap { x =>
      val inflows = x.inflows.find(_._1 == rdii_id)
      if (inflows.nonEmpty) {
        Some(instantToLDT(x.rainfall.index.head), instantToLDT(x.rainfall.index.last)
          , x.rainfall.values.toArray
          , x.flow.values.toArray
          , x.dwp.values.toArray
          , x.inflows.find(_._1 == rdii_id).head._2.values.toArray)
      }
      else None
    }
  }
}


/**
  * Rainfall dependant inflow and infiltration
  */

case class RDIIBuilder(rainfall: TimeSeries[Double], flow: TimeSeries[Double], startDate: LocalDateTime
                       , endDate: LocalDateTime, flowWindows: Seq[Int]) {

  private var limit = 1000
  private var stormSessionWindows: Duration = Duration.ofHours(12)
  private var stormIntensityWindow: Duration = Duration.ofHours(6)
  private var dryDayWindow = Duration.ofHours(48)

  /**
    * Set a number of sessions to find (ordered from maximum)
    */
  def withLimit(l: Int): RDIIBuilder = {
    limit = l
    this
  }

  def withDryDayWindow(window: Duration): RDIIBuilder = {
    dryDayWindow = window
    this
  }

  def withStormSessionWindows(window: Duration): RDIIBuilder = {
    stormSessionWindows = window
    this
  }

  def withStormIntensityWindow(window: Duration): RDIIBuilder = {
    stormIntensityWindow = window
    this
  }

  def build(): RDIIObject = {
    val sd = startDate.toInstant(ZoneOffset.UTC)
    val ed = endDate.plusDays(1).toInstant(ZoneOffset.UTC)

    // This algorithm works only if the series are aligned
    if (rainfall.nonEmpty) {
      val ts = rainfall.slice(rainfall.index.head, ed).join(flow.slice(rainfall.index.head, ed))
      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))

      val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall2, dryDayWindow)

      // Slice data to session
      val sessionDays: Seq[LocalDate] = flow.slice(sd.minus(1, ChronoUnit.DAYS), ed)
        .groupByTime(_.truncatedTo(ChronoUnit.DAYS), _ => identity(0.0))
        .index
        .map(instantToDay)
      //Find dwp for every day in session
      val patternDays: Seq[(LocalDate, Option[LocalDate])] = sessionDays.map(x => (x, findDryDay(x, allDWPDays)))
      //Take flow from dwp
      val patternInflows = patternDays.map(x => (x._1, DryWeatherPattern.get(x._2.getOrElse(LocalDate.MAX), flow)))
        .map { x => x._2.dataPoints.map(t => (LocalDateTime.of(x._1, instantToTime(t._1)), t._2)) }
        .map { x =>
          val xs = x.unzip
          TimeSeries(xs._1.map(_.toInstant(ZoneOffset.UTC)), xs._2)
        }
      val inflows: Seq[(String, TimeSeries[Double])] = flowWindows.map(x => Inflow.fromSession(Session(sd, ed), flow, allDWPDays, Duration.ofMinutes(x)))
        .map(x => x.slice(sd, ed)).map((randomUUID.toString, _))

      val dwp: TimeSeries[Double] = TimeSeriesHelper.concat(patternInflows).slice(sd, ed)
      //Adjust indexes in all series, dwp && inflows already are OK
      val rainfallSection: TimeSeries[Double] = adjust(rainfall2.groupByTime(_.truncatedTo(ChronoUnit.HOURS), _.map(_._2).sum), dwp)
        .filter(x => dwp.index.contains(x._1))
        .slice(sd, ed)
        .addMissing(dwp.resolution, (_, _, _) => 0.0)

      val flowSection: TimeSeries[Double] = flow.slice(sd, ed).filter(x => dwp.index.contains(x._1))

      RDIIObject(rainfallSection, flowSection, dwp, inflows)
    }
    else RDIIObject(TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, Seq())
  }

  def adjust(raw: TimeSeries[Double], template: TimeSeries[Double]): TimeSeries[Double] = {
    val rs = if (raw.nonEmpty && template.nonEmpty) {
      val leftCorrected = if (raw.head.get._1.isAfter(template.head.get._1)) (template.head.get._1, 0.0) else raw.head.get
      val rightCorrected = if (raw.last.get._1.isBefore(template.last.get._1)) (template.last.get._1, 0.0) else raw.last.get
      (leftCorrected +: raw.dataPoints :+ rightCorrected).unzip
    }
    else if (template.nonEmpty) {
      (Vector(template.head.get._1, template.last.get._1), Vector(0.0, 0.0))
    }
    else (Vector(), Vector())

    TimeSeries(rs._1, rs._2)
  }

}
