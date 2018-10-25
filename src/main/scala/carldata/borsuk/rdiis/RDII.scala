package carldata.borsuk.rdiis

import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.borsuk.rdiis.ApiObjects.FitRDIIParams
import carldata.borsuk.rdiis.DryWeatherPattern._
import carldata.borsuk.storms.Storms
import carldata.series.Sessions.Session
import carldata.series.{Gen, TimeSeries}

import scala.collection.immutable
import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.helper.JsonHelper._
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import spray.json._

case class RDIIObject(sessionWindow: Duration, rainfall: TimeSeries[Double], flow: TimeSeries[Double], dwp: TimeSeries[Double]
                      , inflow: TimeSeries[Double], childIds: Seq[String])

class RDII(modelType: String, id: String) {
  var model: immutable.HashMap[String, RDIIObject] = immutable.HashMap.empty[String, RDIIObject]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitRDIIParams): Unit = {

    if (params.flow.values.nonEmpty && params.flow.values.length == params.rainfall.values.length) {
      val edFlow: LocalDateTime = params.flow.startDate.plusSeconds(params.flow.resolution.getSeconds * params.flow.values.length)
      val indexFlow: Seq[Instant] = Gen.mkIndex(dtToInstant(params.flow.startDate), dtToInstant(edFlow), params.flow.resolution)
      val flow: TimeSeries[Double] = TimeSeries(indexFlow.toVector, params.flow.values.toVector)
      val edRainfall: LocalDateTime = params.flow.startDate.plusSeconds(params.flow.resolution.getSeconds * params.flow.values.length)
      val indexRainfall: Seq[Instant] = Gen.mkIndex(dtToInstant(params.flow.startDate), dtToInstant(edRainfall), params.flow.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(indexRainfall.toVector, params.flow.values.toVector)

      val ts = rainfall.slice(rainfall.index.head, dtToInstant(edRainfall)).join(flow.slice(rainfall.index.head, dtToInstant(edRainfall)))
      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))

      val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall2, params.dryDayWindow)

      val rdiis: List[(String, RDIIObject)] = Storms.getAllStorms(rainfall).map(
        x => (x._1, RDIIBuilder(rainfall, flow, instantToLDT(x._2.session.startIndex), instantToLDT(x._2.session.endIndex), allDWPDays)
          .withDryDayWindow(params.dryDayWindow)
          .withStormSessionWindows(x._2.sessionWindow)
          .build())
      )

      model = immutable.HashMap(rdiis: _*)
      buildNumber += 1
    }
  }

  /**
    * List all rdiis with sessionWindow
    */
  def list(sessionWindow: Duration): Seq[(String, LocalDateTime, LocalDateTime)] = {

    val lessOrEqualModel = model.filter(x => x._2.sessionWindow.compareTo(sessionWindow) <= 0)
      .filter(x => x._2.inflow.nonEmpty)
      .toSeq
      .sortBy(_._1)

    val childIds = lessOrEqualModel.flatMap(_._2.childIds).distinct

    lessOrEqualModel.filter(x => !childIds.contains(x._1))
      .map(t => (t._1, instantToLDT(t._2.inflow.index.head), instantToLDT(t._2.inflow.index.last)))
  }

  /**
    * For provided rdii id
    * return series of:
    * rainfall, flow, dwp and rdii
    */
  def get(rdii_id: String): Option[(LocalDateTime, LocalDateTime
    , Array[Double], Array[Double], Array[Double], Array[Double])] = {
    if (model.contains(rdii_id))
      model.filter(_._1 == rdii_id)
        .map { x =>
          if (x._2.inflow.nonEmpty) Some(instantToLDT(x._2.rainfall.index.head)
            , instantToLDT(x._2.rainfall.index.last)
            , x._2.rainfall.values.toArray
            , x._2.flow.values.toArray
            , x._2.dwp.values.toArray
            , x._2.inflow.values.toArray)
          else None
        }.head
    else None
  }
}

/**
  * Rainfall dependant inflow and infiltration
  */
case class RDIIBuilder(rainfall: TimeSeries[Double], flow: TimeSeries[Double], startDate: LocalDateTime
                       , endDate: LocalDateTime, allDWPDays: Seq[LocalDate]) {

  private var stormSessionWindows: Duration = Duration.ofHours(12)
  private var dryDayWindow = Duration.ofHours(48)

  def withDryDayWindow(window: Duration): RDIIBuilder = {
    dryDayWindow = window
    this
  }

  def withStormSessionWindows(window: Duration): RDIIBuilder = {
    stormSessionWindows = window
    this
  }

  def build(): RDIIObject = {
    val sd = startDate.toInstant(ZoneOffset.UTC)
    val ed = endDate.plusDays(1).toInstant(ZoneOffset.UTC)

    // This algorithm works only if the series are aligned
    if (rainfall.nonEmpty) {
      val ts = rainfall.slice(rainfall.index.head, ed).join(flow.slice(rainfall.index.head, ed))
      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))

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
      val inflow: TimeSeries[Double] = Inflow.fromSession(Session(sd, ed), flow, allDWPDays)

      val dwp: TimeSeries[Double] = TimeSeriesHelper.concat(patternInflows).slice(sd, ed)
      //Adjust indexes in all series, dwp && inflows already are OK
      val rainfallSection: TimeSeries[Double] = adjust(rainfall2.groupByTime(_.truncatedTo(ChronoUnit.HOURS), _.map(_._2).sum), dwp)
        .filter(x => dwp.index.contains(x._1))
        .slice(sd, ed)
        .addMissing(dwp.resolution, (_, _, _) => 0.0)

      val flowSection: TimeSeries[Double] = flow.slice(sd, ed).filter(x => dwp.index.contains(x._1))

      RDIIObject(stormSessionWindows, rainfallSection, flowSection, dwp, inflow, Seq())
    }
    else RDIIObject(Duration.ZERO, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, Seq())
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


/**
  * RDII Object JSON serialization
  */
object RDIIObjectJsonProtocol extends DefaultJsonProtocol {

  implicit object RDIIObjectFormat extends  RootJsonFormat[RDIIObject] {
     def read(json: JsValue): RDIIObject = json match {

       case JsObject(x) =>
         RDIIObject(Duration.ZERO,TimeSeries.empty,TimeSeries.empty, TimeSeries.empty, TimeSeries.empty,Seq())
       case _ => RDIIObject(Duration.ZERO,TimeSeries.empty,TimeSeries.empty, TimeSeries.empty, TimeSeries.empty,Seq())
     }

    override def write(obj: RDIIObject): JsObject = {
      JsObject(
        "session-window" -> JsString(obj.sessionWindow.toString),
        "rainfall" -> TimeSeriesParams(instantToLDT(obj.rainfall.index.head),obj.rainfall.resolution, obj.rainfall.values.toArray).toJson,
        "flow" -> TimeSeriesParams(instantToLDT(obj.flow.index.head),obj.flow.resolution, obj.flow.values.toArray).toJson,
        "dwp" -> TimeSeriesParams(instantToLDT(obj.dwp.index.head),obj.dwp.resolution, obj.dwp.values.toArray).toJson,
        "inflow" -> TimeSeriesParams(instantToLDT(obj.inflow.index.head),obj.inflow.resolution, obj.inflow.values.toArray).toJson,
        "child-ids" ->  JsArray(obj.childIds.map(_.toJson).toVector)
      )
    }
  }

}

