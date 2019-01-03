package carldata.borsuk.rdiis

import java.nio.file.Paths
import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.{Model, PVCHelper, TimeSeriesHelper}
import carldata.borsuk.rdiis.ApiObjects.FitRDIIParams
import carldata.borsuk.rdiis.DryWeatherPattern._
import carldata.borsuk.storms.Storms
import carldata.borsuk.storms.Storms.StormParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}
import org.slf4j.LoggerFactory
import spray.json._

import scala.collection.immutable


class RDII(modelType: String, id: String) {

  private val Log = LoggerFactory.getLogger("RDII")
  var model: immutable.HashMap[String, RDIIObject] = immutable.HashMap.empty[String, RDIIObject]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitRDIIParams): Unit = {

    Log.debug("Start Fit model: " + this.id)
    if (params.flow.values.nonEmpty && params.rainfall.values.nonEmpty) {
      val edFlow: LocalDateTime = params.flow.startDate.plusSeconds(params.flow.resolution.getSeconds * params.flow.values.length)
      val indexFlow: Seq[Instant] = Gen.mkIndex(dtToInstant(params.flow.startDate), dtToInstant(edFlow), params.flow.resolution)
      val flow: TimeSeries[Double] = TimeSeries(indexFlow.toVector, params.flow.values.toVector).filter(_._2 >= 0)
      val edRainfall: LocalDateTime = params.rainfall.startDate.plusSeconds(params.rainfall.resolution.getSeconds * params.rainfall.values.length)
      val indexRainfall: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(edRainfall), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(indexRainfall.toVector, params.rainfall.values.toVector).filter(_._2 >= 0)
      val minSessionWindow = if (params.minSessionWindow == Duration.ZERO) rainfall.resolution else params.minSessionWindow

      val ts = TimeSeriesHelper.slice(rainfall, rainfall.index.head, dtToInstant(edRainfall))
        .join(TimeSeriesHelper.slice(flow, rainfall.index.head, dtToInstant(edRainfall)))

      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))

      // This code is for remembering how to use it
      //Log.debug("flow: " + SizeEstimator.estimate(flow))
      // memory info
      //val mb = 1024*1024
      //val runtime = Runtime.getRuntime
      //Log.debug("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
      //Log.debug("** Free Memory:  " + runtime.freeMemory / mb)
      //Log.debug("** Total Memory: " + runtime.totalMemory / mb)
      //Log.debug("** Max Memory:   " + runtime.maxMemory / mb)
      //Log.debug("** Available Processors:   " + runtime.availableProcessors())

      val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall2, params.dryDayWindow)

      val baseSessions: List[(Int, StormParams)] = Sessions.findSessions(rainfall, minSessionWindow)
        .zipWithIndex
        .map(x =>
          x._2 ->
            StormParams(x._1, rainfall.resolution, TimeSeriesHelper.slice(rainfall, x._1.startIndex
              , x._1.endIndex.plusSeconds(rainfall.resolution.getSeconds)).values, Seq())
        ).toList
      val highestIndex = baseSessions.map(_._1).max

      val storms = if (baseSessions != Nil) {
        val listOfSessionWindows: Seq[Duration] =
          baseSessions.map(x => x._2.session.endIndex).zip(baseSessions.tail.map(x => x._2.session.startIndex))
            .map(x => Duration.between(x._1, x._2))
            .distinct.sorted

        val maxSessionWindow = if (params.maxSessionWindow == Duration.ZERO) listOfSessionWindows.last
        else params.maxSessionWindow

        Storms.mergeSessions(baseSessions, baseSessions.toSet, listOfSessionWindows.filter(d => d.compareTo(maxSessionWindow) <= 0)
          , rainfall.resolution, highestIndex)
      }
      else List()

      val rdiis: List[(String, RDIIObject)] = storms.map {
        x =>

          (x._1.toString, RDIIBuilder(rainfall2, flow, instantToLDT(x._2.session.startIndex), instantToLDT(x._2.session.endIndex), allDWPDays)
            .withDryDayWindow(params.dryDayWindow)
            .withStormSessionWindows(x._2.sessionWindow)
            .build())
      }

      model = immutable.HashMap(rdiis: _*)
      buildNumber += 1
      save()
      Log.debug("Stop Fit model: " + this.id)
    }
  }

  def save() {
    Log.debug("Save model: " + this.id)
    val path = Paths.get("/borsuk_data/rdiis/", this.modelType)
    val rDIIFileContent = new RDIIFileContent(this.model, buildNumber)
    val model = Model(this.modelType, this.id, rDIIFileContent.toJson(RDIIFileContentJsonProtocol.RDIIFileContentFormat).toString)
    PVCHelper.saveModel(path, model)
    Log.debug("Model: " + this.id + " saved")
  }

  /**
    * List all rdiis with sessionWindow
    */
  def list(sessionWindow: Duration): Seq[(String, LocalDateTime, LocalDateTime)] = {
    Log.debug("List for model: " + this.id)
    val lessOrEqualModel = model.filter(x => x._2.sessionWindow.compareTo(sessionWindow) <= 0)
      .filter(x => x._2.inflow.nonEmpty)
      .toSeq
      .sortBy(_._1)

    lessOrEqualModel.map { x =>
      (x._1, instantToLDT(x._2.session.startIndex), instantToLDT(x._2.session.endIndex))
    }
  }

  /**
    * For provided rdii id
    * return series of:
    * rainfall, flow, dwp and rdii
    */
  def get(rdii_id: String): Option[(LocalDateTime, LocalDateTime
    , Array[Double], Array[Double], Array[Double], Array[Double])] = {
    Log.debug("Get model: " + this.id + " with RDII_ID: " + rdii_id)
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

  val inflow: TimeSeries[Double] = Inflow.fromSession(Session(startDate.minusDays(1).toInstant(ZoneOffset.UTC)
    , endDate.plusDays(2).toInstant(ZoneOffset.UTC)), flow, allDWPDays)

  def withDryDayWindow(window: Duration): RDIIBuilder = {
    dryDayWindow = window
    this
  }

  def withStormSessionWindows(window: Duration): RDIIBuilder = {
    stormSessionWindows = window
    this
  }

  def build(): RDIIObject = {
    // This algorithm works only if the series are aligned
    if (rainfall.nonEmpty) {
      val sd = startDate.minusDays(1).toInstant(ZoneOffset.UTC)
      val ed = endDate.plusDays(2).toInstant(ZoneOffset.UTC)
      // Slice data to session
      val sessionDays: Seq[LocalDate] = TimeSeriesHelper.slice(flow, sd.minus(1, ChronoUnit.DAYS), ed)
        .groupByTime(_.truncatedTo(ChronoUnit.DAYS), _ => identity(0.0))
        .index
        .map(instantToDay)
      //Find dwp for every day in session
      val patternDays: Seq[(LocalDate, Option[LocalDate])] = sessionDays.map(x => (x, findDryDay(x, allDWPDays)))
      //Take flow from dwp
      val patternInflows = patternDays.map(x => (x._1, DryWeatherPattern.get(x._2.getOrElse(LocalDate.MAX), flow)))
        .map(x => (x._1, TimeSeries.interpolate(x._2, x._2.resolution)))
        .map { x =>
          x._2.dataPoints.map(dt => (LocalDateTime.of(x._1, instantToTime(dt._1)), dt._2))
        }
        .map { x =>
          val xs = x.unzip
          TimeSeries(xs._1.map(_.toInstant(ZoneOffset.UTC)), xs._2)
        }
      val slicedInflow: TimeSeries[Double] = TimeSeriesHelper.slice(TimeSeries.interpolate(inflow, inflow.resolution), sd, ed.plus(inflow.resolution))
      val (shiftedSd, shiftedEd) = if (slicedInflow.nonEmpty) (slicedInflow.index.head, slicedInflow.index.last.plus(slicedInflow.resolution)) else (sd, ed)
      val dwp: TimeSeries[Double] = TimeSeriesHelper.slice(TimeSeriesHelper.concat(patternInflows), shiftedSd, shiftedEd)
      //Adjust indexes in all series, dwp && inflows already are OK
      val rainfallSection: TimeSeries[Double] = if (dwp.isEmpty) TimeSeries.empty else
        TimeSeriesHelper.slice(
          TimeSeriesHelper.slice(rainfall,
            startDate.minusMonths(3).toInstant(ZoneOffset.UTC), shiftedEd.plus(1, ChronoUnit.HOURS))
            .groupByTime(_.truncatedTo(ChronoUnit.HOURS), _.map(_._2).sum)
            .addMissing(dwp.resolution, (_, _, _) => 0.0)
            .filter(x => dwp.index.contains(x._1))
          , shiftedSd, shiftedEd)

      val flowSection: TimeSeries[Double] = TimeSeriesHelper.slice(flow, shiftedSd, shiftedEd).filter(x => dwp.index.contains(x._1))

      RDIIObject(sessionWindow = stormSessionWindows
        , rainfall = rainfallSection
        , flow = flowSection
        , dwp = dwp
        , inflow = slicedInflow
        , session = Session(shiftedSd.plus(1, ChronoUnit.DAYS)
          , shiftedEd.minus(1, ChronoUnit.DAYS).minus(slicedInflow.resolution)))
    }
    else {
      RDIIObject(Duration.ZERO, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty
        , TimeSeries.empty, Session(Instant.EPOCH, Instant.EPOCH))
    }
  }
}

