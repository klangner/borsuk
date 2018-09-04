package carldata.borsuk

import java.time.LocalDateTime

import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.helper.JsonHelper._
import spray.json._

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

  /**
    * RDIIObject formatter
    */
  implicit object RDIIObjectFormat extends RootJsonFormat[RDIIObject] {
    def write(rdii: RDIIObject): JsObject = {
      JsObject(
        "id" -> rdii.id.toJson,
        "start-date" -> rdii.startDate.toString.toJson,
        "end-date" -> rdii.endDate.toString.toJson
      )
    }

    def read(json: JsValue): RDIIObject = {
      val fields = json.asJsObject.fields
      val id: String = fields("id").toString
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("startDate").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("endDate").toString)
      RDIIObject(id, startDate, endDate)
    }


  }


  /**
    * Model List formatter
    */
  implicit object ListResponseFormat extends RootJsonFormat[ListResponse] {
    def write(status: ListResponse): JsObject = {
      JsObject(
        "rdii" -> JsArray(status.rdii.map {
          _.toJson
        }.toVector)
      )
    }

    def read(value: JsValue): ListResponse = {
      value.asJsObject().fields("rdii") match {
        case JsArray(arr) => {
          ListResponse(arr.map { a =>
            a.convertTo[RDIIObject]
          }.toArray)
        }
        case _ => ListResponse(Array())
      }
    }
  }

  /**
    * Model List formatter
    */
  implicit object GetResponseFormat extends RootJsonFormat[GetResponse] {
    def write(response: GetResponse): JsObject = {
      JsObject(
        "start-date" -> JsString(response.startDate.toString),
        "end-date" -> JsString(response.endDate.toString),
        "flow" -> JsArray(response.flow.map(_.toJson).toVector),
        "rainfall" -> JsArray(response.flow.map(_.toJson).toVector),
        "dwp" -> JsArray(response.flow.map(_.toJson).toVector),
        "rdii" -> JsArray(response.flow.map(_.toJson).toVector))
    }

    def read(value: JsValue): GetResponse = {
      val fields = value.asJsObject.fields
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val flow = fields("flow").convertTo[Array[Double]]
      val rainfall = fields("rainfall").convertTo[Array[Double]]
      val dwp = fields("dwp").convertTo[Array[Double]]
      val rdii = fields("rdii").convertTo[Array[Double]]
      GetResponse(startDate, endDate, flow, rainfall, dwp, rdii)
    }
  }

}