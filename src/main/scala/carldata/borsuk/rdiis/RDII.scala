package carldata.borsuk.rdiis

import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.helper.JsonHelper._
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.borsuk.rdiis.ApiObjects.FitRDIIParams
import carldata.borsuk.rdiis.DryWeatherPattern._
import carldata.borsuk.storms.Storms
import carldata.series.Sessions.Session
import carldata.series.{Gen, TimeSeries}
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.HashMap

case class RDIIObject(sessionWindow: Duration, rainfall: TimeSeries[Double], flow: TimeSeries[Double], dwp: TimeSeries[Double]
                      , inflow: TimeSeries[Double], childIds: Seq[String])

class RDII(modelType: String, id: String) {
  var model: immutable.HashMap[String, RDIIObject] = immutable.HashMap.empty[String, RDIIObject]
  var buildNumber: Int = 0

  /** Fit model */
  def fit(params: FitRDIIParams): Unit = {

    if (params.flow.values.nonEmpty && params.rainfall.values.nonEmpty) {
      val edFlow: LocalDateTime = params.flow.startDate.plusSeconds(params.flow.resolution.getSeconds * params.flow.values.length)
      val indexFlow: Seq[Instant] = Gen.mkIndex(dtToInstant(params.flow.startDate), dtToInstant(edFlow), params.flow.resolution)
      val flow: TimeSeries[Double] = TimeSeries(indexFlow.toVector, params.flow.values.toVector)
      val edRainfall: LocalDateTime = params.rainfall.startDate.plusSeconds(params.rainfall.resolution.getSeconds * params.rainfall.values.length)
      val indexRainfall: Seq[Instant] = Gen.mkIndex(dtToInstant(params.rainfall.startDate), dtToInstant(edRainfall), params.rainfall.resolution)
      val rainfall: TimeSeries[Double] = TimeSeries(indexRainfall.toVector, params.rainfall.values.toVector)

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
      JsObject(
        "session-window" -> JsString(obj.sessionWindow.toString),
        "rainfall" -> TimeSeriesParams(instantToLDT(obj.rainfall.index.head), obj.rainfall.resolution, obj.rainfall.values.toArray).toJson,
        "flow" -> TimeSeriesParams(instantToLDT(obj.flow.index.head), obj.flow.resolution, obj.flow.values.toArray).toJson,
        "dwp" -> TimeSeriesParams(instantToLDT(obj.dwp.index.head), obj.dwp.resolution, obj.dwp.values.toArray).toJson,
        "inflow" -> TimeSeriesParams(instantToLDT(obj.inflow.index.head), obj.inflow.resolution, obj.inflow.values.toArray).toJson,
        "child-ids" -> JsArray(obj.childIds.map(_.toJson).toVector)
      )
    }
  }

  def convertTimeSeriesParamsToTimeSeries(tsp: TimeSeriesParams): TimeSeries[Double] = {
    if (tsp.values.isEmpty) {
      TimeSeries.empty
    } else {
      val index = (0 until tsp.values.length).map(
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
