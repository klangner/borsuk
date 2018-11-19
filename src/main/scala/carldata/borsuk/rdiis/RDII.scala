package carldata.borsuk.rdiis

import java.nio.file.{Files, Paths}
import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.JsonHelper._
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.borsuk.rdiis.ApiObjects.FitRDIIParams
import carldata.borsuk.rdiis.DryWeatherPattern._
import carldata.borsuk.storms.Storms
import carldata.borsuk.storms.Storms.StormParams
import carldata.series.Sessions.Session
import carldata.series.{Gen, Sessions, TimeSeries}
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.HashMap

case class RDIIObject(sessionWindow: Duration, rainfall: TimeSeries[Double], flow: TimeSeries[Double], dwp: TimeSeries[Double]
                      , inflow: TimeSeries[Double], childIds: Seq[String])

class RDII(modelType: String, id: String) {

  import RDIIObjectHashMapJsonProtocol._

  var model: immutable.HashMap[String, RDIIObject] = immutable.HashMap.empty[String, RDIIObject]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitRDIIParams): Unit = {

    if (params.flow.values.nonEmpty && params.rainfall.values.nonEmpty) {
      val edFlow: LocalDateTime = params.flow.startDate.plusSeconds(params.flow.resolution.getSeconds * params.flow.values.length)
      val indexFlow: Seq[Instant] = Gen.mkIndex(dtToInstant(params.flow.startDate), dtToInstant(edFlow), params.flow.resolution)
      val flow: TimeSeries[Double] = TimeSeries(indexFlow.toVector, params.flow.values.toVector).filter(_._2 >= 0)
      val edRainfall: LocalDateTime = params.rainfall.startDate.plusSeconds(params.rainfall.resolution.getSeconds * params.rainfall.values.length)
      val indexRainfall: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(edRainfall), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(indexRainfall.toVector, params.rainfall.values.toVector).filter(_._2 >= 0)
      val minSessionWindow = if (params.minSessionWindow == Duration.ZERO) rainfall.resolution else params.minSessionWindow

      val ts = rainfall.slice(rainfall.index.head, dtToInstant(edRainfall)).join(flow.slice(rainfall.index.head, dtToInstant(edRainfall)))
      val rainfall2 = TimeSeries(ts.index, ts.values.map(_._1))

      val allDWPDays: Seq[LocalDate] = DryWeatherPattern.findAllDryDays(rainfall2, params.dryDayWindow)

      val baseSessions: List[(String, StormParams)] = Sessions.findSessions(rainfall, minSessionWindow)
        .zipWithIndex
        .map(x =>
          x._2.toString ->
            StormParams(x._1, rainfall.resolution, rainfall.slice(x._1.startIndex
              , x._1.endIndex.plusSeconds(rainfall.resolution.getSeconds)).values, Seq())
        ).toList

      val storms = if (baseSessions != Nil) {
        val listOfSessionWindows: Seq[Duration] =
          baseSessions.map(x => x._2.session.endIndex).zip(baseSessions.tail.map(x => x._2.session.startIndex))
            .map(x => Duration.between(x._1, x._2))
            .distinct.sorted

        val maxSessionWindow = if (params.maxSessionWindow == Duration.ZERO) listOfSessionWindows.last else params.maxSessionWindow
        Storms.mergeSessions(baseSessions, baseSessions, listOfSessionWindows.filter(d => d.compareTo(maxSessionWindow) <= 0), rainfall.resolution)
      }
      else List()

      //println(LocalDateTime.now + " - " + storms.length.toString)
      //var i = 1
      val rdiis: List[(String, RDIIObject)] = storms.map {
        x =>
          //println(LocalDateTime.now + " - build model no: " + i + " storm value count: " + x._2.values.length)
          //i += 1
          (x._1, RDIIBuilder(rainfall2, flow, instantToLDT(x._2.session.startIndex), instantToLDT(x._2.session.endIndex), allDWPDays)
            .withDryDayWindow(params.dryDayWindow)
            .withStormSessionWindows(x._2.sessionWindow)
            .build())
      }

      model = immutable.HashMap(rdiis: _*)
      save()
      buildNumber += 1
    }
  }

  def save() {
    val path = Paths.get("/borsuk_data/rdiis/", this.modelType)
    val filePath = Paths.get(path.toString, this.id)
    if (Files.exists(path)) {
      Files.write(filePath, this.model.toJson(RDIIObjectHashMapFormat).toString.getBytes)
    } else {
      Files.createDirectories(path)
      Files.write(filePath, this.model.toJson(RDIIObjectHashMapFormat).toString.getBytes)
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
      val inflow: TimeSeries[Double] = Inflow.fromSession(Session(sd, ed), flow, allDWPDays).slice(sd, ed)

      val (shiftedSd, shiftedEd) = if (inflow.nonEmpty) (inflow.index.head, inflow.index.last.plus(inflow.resolution)) else (sd, ed)

      val dwp: TimeSeries[Double] = TimeSeriesHelper.concat(patternInflows).slice(shiftedSd, shiftedEd)
      //Adjust indexes in all series, dwp && inflows already are OK
      val rainfallSection: TimeSeries[Double] = adjust(
        rainfall.slice(startDate.minusMonths(3).toInstant(ZoneOffset.UTC), shiftedEd)
          .groupByTime(_.truncatedTo(ChronoUnit.HOURS), _.map(_._2).sum), dwp)
        .filter(x => dwp.index.contains(x._1))
        .slice(shiftedSd, shiftedEd)
        .addMissing(dwp.resolution, (_, _, _) => 0.0)

      val flowSection: TimeSeries[Double] = flow.slice(shiftedSd, shiftedEd).filter(x => dwp.index.contains(x._1))

      RDIIObject(stormSessionWindows, rainfallSection, flowSection, dwp, inflow, Seq())

    }
    else RDIIObject(Duration.ZERO, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, Seq())
  }

  def adjust(raw: TimeSeries[Double], template: TimeSeries[Double]): TimeSeries[Double] = {
    val rs = if (raw.nonEmpty && template.nonEmpty) {
      val leftCorrected = if (raw.head.get._1.isAfter(template.head.get._1)) (template.head.get._1, 0.0) else raw.head.get
      val rightCorrected = if (raw.last.get._1.isBefore(template.last.get._1)) (template.last.get._1, 0.0) else raw.last.get
      (leftCorrected +: raw.dataPoints :+ rightCorrected).distinct.unzip
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

  import carldata.borsuk.BasicApiObjects._
  import spray.json._

  implicit object RDIIObjectFormat extends RootJsonFormat[RDIIObject] {
    def read(json: JsValue): RDIIObject = json match {

      case JsObject(x) =>

        val rainfallParams: TimeSeriesParams = x.get("rainfall")
          .map(_.convertTo[TimeSeriesParams])
          .getOrElse(TimeSeriesParams(LocalDateTime.now, Duration.ofSeconds(0), Array()))

        val flowParams: TimeSeriesParams = x.get("flow")
          .map(_.convertTo[TimeSeriesParams])
          .getOrElse(TimeSeriesParams(LocalDateTime.now, Duration.ofSeconds(0), Array()))

        val dwpParams: TimeSeriesParams = x.get("dwp")
          .map(_.convertTo[TimeSeriesParams])
          .getOrElse(TimeSeriesParams(LocalDateTime.now, Duration.ofSeconds(0), Array()))

        val inflow: TimeSeriesParams = x.get("inflow")
          .map(_.convertTo[TimeSeriesParams])
          .getOrElse(TimeSeriesParams(LocalDateTime.now, Duration.ofSeconds(0), Array()))

        RDIIObject(Duration.parse(x.get("session-window").map(stringFromValue).get),
          convertTimeSeriesParamsToTimeSeries(rainfallParams),
          convertTimeSeriesParamsToTimeSeries(flowParams),
          convertTimeSeriesParamsToTimeSeries(dwpParams),
          convertTimeSeriesParamsToTimeSeries(inflow),
          x("child-ids").convertTo[Array[String]]
        )
      case _ => RDIIObject(Duration.ZERO, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, TimeSeries.empty, Seq())
    }

    def write(obj: RDIIObject): JsObject = {

      val tspRainfall = if (obj.rainfall.isEmpty) TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array()) else
        TimeSeriesParams(instantToLDT(obj.rainfall.index.head), obj.rainfall.resolution, obj.rainfall.values.toArray)

      val tspFlow = if (obj.flow.isEmpty) TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array()) else
        TimeSeriesParams(instantToLDT(obj.flow.index.head), obj.flow.resolution, obj.flow.values.toArray)

      val tspDwp = if (obj.dwp.isEmpty) TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array()) else
        TimeSeriesParams(instantToLDT(obj.dwp.index.head), obj.dwp.resolution, obj.dwp.values.toArray)

      val tspInflow = if (obj.inflow.isEmpty) TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array()) else
        TimeSeriesParams(instantToLDT(obj.inflow.index.head), obj.inflow.resolution, obj.inflow.values.toArray)
      val childs = if (obj.childIds == Nil) Vector() else  obj.childIds.map(_.toJson).toVector

      JsObject(
        "session-window" -> JsString(obj.sessionWindow.toString),
        "rainfall" -> tspRainfall.toJson,
        "flow" -> tspFlow.toJson,
        "dwp" -> tspDwp.toJson,
        "inflow" -> tspInflow.toJson,
        "child-ids" -> JsArray(childs)
      )
    }
  }

  def convertTimeSeriesParamsToTimeSeries(tsp: TimeSeriesParams): TimeSeries[Double] = {
    if (tsp.values.isEmpty) {
      TimeSeries.empty
    } else {
      val index = tsp.values.indices.map(
        x => dtToInstant(tsp.startDate.plus(tsp.resolution.multipliedBy(x)))
      ).toVector
      TimeSeries(index, tsp.values.toVector)
    }
  }
}

/**
  * HashMap with RDII Objects formatter
  */

object RDIIObjectHashMapJsonProtocol extends DefaultJsonProtocol {

  import RDIIObjectJsonProtocol._
  import spray.json._

  implicit object RDIIObjectHashMapFormat extends RootJsonFormat[immutable.HashMap[String, RDIIObject]] {
    def read(json: JsValue): HashMap[String, RDIIObject] = {
      val map = json.asInstanceOf[JsArray].elements
        .map(jsVal => (stringFromValue(jsVal.asJsObject.fields("key")),
          jsVal.asJsObject.fields("value").convertTo[RDIIObject])).toMap

      val hash = immutable.HashMap.empty
      hash.++(map)
    }

    def write(obj: HashMap[String, RDIIObject]): JsValue = {
      JsArray(obj.map(x => JsObject(
        "key" -> JsString(x._1),
        "value" -> x._2.toJson
      )).toVector)
    }
  }

}
