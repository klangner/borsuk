package carldata.borsuk.rdiis

import java.time.{Duration, LocalDateTime}
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.helper.JsonHelper.stringFromValue
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.series.TimeSeries
import spray.json.DefaultJsonProtocol

import scala.collection.immutable
import scala.collection.immutable.HashMap

case class RDIIObject(sessionWindow: Duration, rainfall: TimeSeries[Double], flow: TimeSeries[Double], dwp: TimeSeries[Double]
                      , inflow: TimeSeries[Double], childIds: Seq[String])


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

      val childs = if (obj.childIds == Nil) Vector() else obj.childIds.map(_.toJson).toVector

      JsObject(
        "session-window" -> JsString(obj.sessionWindow.toString),
        "rainfall" -> TimeSeriesHelper.toTimeSeriesParams(obj.rainfall).toJson,
        "flow" -> TimeSeriesHelper.toTimeSeriesParams(obj.flow).toJson,
        "dwp" -> TimeSeriesHelper.toTimeSeriesParams(obj.dwp).toJson,
        "inflow" -> TimeSeriesHelper.toTimeSeriesParams(obj.inflow).toJson,
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
