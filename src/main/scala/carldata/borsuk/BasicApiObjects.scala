package carldata.borsuk

import java.time.{Duration, LocalDateTime}

import carldata.borsuk.helper.JsonHelper._
import spray.json._

/**
  * Here are definition of objects used in REST API with their json serialization
  */
object BasicApiObjects {

  case class TimeSeriesParams(startDate: LocalDateTime, resolution: Duration, values: Array[Double])

}

/**
  * JSON serialization
  */
object BasicApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import BasicApiObjects._

  /**
    * CreateRDIIParams formatter
    */
  implicit object TimeSeriesParamsFormat extends RootJsonFormat[TimeSeriesParams] {
    def write(params: TimeSeriesParams): JsObject = {
      JsObject(
        "start-date" -> JsString(params.startDate.toString),
        "resolution" -> JsString(params.resolution.toString),
        "values" -> JsArray(params.values.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): TimeSeriesParams = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val resolution = request.get("resolution").map(stringFromValue).map(Duration.parse).getOrElse(Duration.ofHours(1))
        val values = request.get("values").map(arrayFromValue(_, doubleFromValue)).getOrElse(Array.empty[Double])

        TimeSeriesParams(startDate, resolution, values)
      case _ => TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())
    }
  }

}