package carldata.borsuk

import java.time.LocalDateTime
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import spray.json._
import helper.JsonHelper._
/**
  * Here are definition of objects used in REST API with their json serialization
  */
object RDIIApiObjects {

  case class CreateRDIIParams(modelType: String)

  case class ModelCreatedResponse(id: String)

  case class FitRDIIParams(startDate: LocalDateTime, flow: Array[Double], rainfall: Array[Double], window: Array[Int])

  case class ListRequest(startDate: LocalDateTime, endDate: LocalDateTime, stormSessionWindows: Int
                         , stormIntensityWindow: Int, dryDayWindow: Int)

  case class RDIIObject(id: String, startDate: LocalDateTime, endDate: LocalDateTime)

  case class ListResponse(rdii: Array[RDIIObject])

  case class GetRequest(startDate: LocalDateTime, endDate: LocalDateTime, stormSessionWindows: Int
                        , stormIntensityWindow: Int, dryDayWindow: Int)


  case class GetResponse(startDate: LocalDateTime, endDate: LocalDateTime, flow: Array[Double]
                         , rainfall: Array[Double], dwp: Array[Double], rdii: Array[Double])

  case class ModelStatus(build: Int)

}

/**
  * JSON serialization
  */
object RDIIApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import RDIIApiObjects._
  import akka.stream.Materializer
  implicit val system = ActorSystem(getClass.getSimpleName)
  implicit val materializer = ActorMaterializer()
  /**
    * CreateRDIIParams formatter
    */
  implicit object CreateRDIIParamsFormat extends RootJsonFormat[CreateRDIIParams] {
    def write(params: CreateRDIIParams): JsObject = {
      JsObject(
        "type" -> JsString(params.modelType)
      )
    }

    def read(value: JsValue): CreateRDIIParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        CreateRDIIParams(modelType)
      case _ => CreateRDIIParams("daily-pattern-v0")
    }
  }

  /**
    * ModelCreatedResponse formatter
    */
  implicit object ModelCreatedResponseJsonFormat extends RootJsonFormat[ModelCreatedResponse] {
    def write(response: ModelCreatedResponse): JsObject = {
      JsObject(
        "id" -> JsString(response.id)
      )
    }

    def read(value: JsValue): ModelCreatedResponse = value match {
      case JsObject(response) =>
        val id = response.get("id").map(stringFromValue).getOrElse("")
        ModelCreatedResponse(id)
      case _ => ModelCreatedResponse("")
    }
  }

  /**
    * FitParams formatter
    */
  implicit object FitParamsFormat extends RootJsonFormat[FitRDIIParams] {
    def write(params: FitRDIIParams): JsObject = {
      JsObject(
        "start-date" -> JsString(params.startDate.toString),
        "flow" -> JsArray(params.flow.map(JsNumber(_)).toVector),
        "rainfall" -> JsArray(params.rainfall.map(JsNumber(_)).toVector),
        "window" -> JsArray(params.window.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): FitRDIIParams = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val flow = request.get("flow").map(arrayFromValue).map(_.map(doubleFromValue)).getOrElse(Array.empty[Double])
        val rainfall = request.get("rainfall").map(arrayFromValue).map(_.map(doubleFromValue)).getOrElse(Array.empty[Double])
        val window = request.get("window").map(arrayFromValue).map(_.map(intFromValue)).getOrElse(Array.empty[Int])
        FitRDIIParams(startDate, flow, rainfall, window)
      case _ => FitRDIIParams(LocalDateTime.now(), Array.empty, Array.empty, Array.empty)
    }
  }

  /**
    * ModelStatus formatter
    */
  implicit object ModelStatusFormat extends RootJsonFormat[ModelStatus] {
    def write(status: ModelStatus): JsObject = {
      JsObject(
        "build" -> JsNumber(status.build)
      )
    }

    def read(value: JsValue): ModelStatus = value match {
      case JsObject(request) =>
        val build = request.get("build").map(doubleFromValue).map(_.toInt).getOrElse(0)
        ModelStatus(build)
      case _ => ModelStatus(0)
    }
  }




}
