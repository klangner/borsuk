package carldata.borsuk.rdiis

import java.time.{Duration, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.helper.JsonHelper._
import spray.json._

/**
  * Here are definition of objects used in REST API with their json serialization
  */
object ApiObjects {

  case class CreateParams(modelType: String, id: String)

  case class ModelCreatedResponse(id: String)

  case class FitRDIIParams(flow: TimeSeriesParams, rainfall: TimeSeriesParams)

  case class RDIIObject(id: String, startDate: LocalDateTime, endDate: LocalDateTime)

  case class ListResponse(rdii: Array[RDIIObject])

  case class GetResponse(startDate: LocalDateTime, endDate: LocalDateTime, flow: Array[Double]
                         , rainfall: Array[Double], dwp: Array[Double], rdii: Array[Double])

  case class ModelStatus(build: Int)

}

/**
  * JSON serialization
  */
object ApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import ApiObjects._

  /**
    * CreateRDIIParams formatter
    */
  implicit object CreateRDIIParamsFormat extends RootJsonFormat[CreateParams] {
    def write(params: CreateParams): JsObject = {
      JsObject(
        "type" -> JsString(params.modelType),
        "id" -> JsString(params.id)
      )
    }

    def read(value: JsValue): CreateParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        val id = request.get("id").map(stringFromValue).getOrElse(randomUUID().toString)
        CreateParams(modelType, id)
      case _ => CreateParams("daily-pattern-v0", randomUUID().toString)
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
  implicit object FitRDIIParamsFormat extends RootJsonFormat[FitRDIIParams] {
    val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())

    def write(params: FitRDIIParams): JsObject = {
      JsObject(
        "flow" -> params.flow.toJson,
        "rainfall" -> params.rainfall.toJson
      )
    }

    def read(value: JsValue): FitRDIIParams = value match {

      case JsObject(x) =>
        FitRDIIParams(
          x.get("flow").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP),
          x.get("rainfall").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
        )
      case _ => FitRDIIParams(emptyTSP, emptyTSP)
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
  implicit object RDIIObjectFormat extends RootJsonFormat[ApiObjects.RDIIObject] {
    def write(rdii: ApiObjects.RDIIObject): JsObject = {
      JsObject(
        "id" -> rdii.id.toJson,
        "start-date" -> rdii.startDate.toString.toJson,
        "end-date" -> rdii.endDate.toString.toJson
      )
    }

    def read(json: JsValue): ApiObjects.RDIIObject = {
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
        case JsArray(arr) =>
          ListResponse(arr.map { a =>
            a.convertTo[ApiObjects.RDIIObject]
          }.toArray)
        case _ => ListResponse(Array())
      }
    }
  }

  /**
    * Get response formatter
    */
  implicit object GetResponseFormat extends RootJsonFormat[GetResponse] {
    def write(response: GetResponse): JsObject = {
      JsObject(
        "start-date" -> JsString(response.startDate.toString),
        "end-date" -> JsString(response.endDate.toString),
        "flow" -> JsArray(response.flow.map(_.toJson).toVector),
        "rainfall" -> JsArray(response.rainfall.map(_.toJson).toVector),
        "dwp" -> JsArray(response.dwp.map(_.toJson).toVector),
        "rdii" -> JsArray(response.rdii.map(_.toJson).toVector))
    }

    def read(value: JsValue): GetResponse = {
      val fields = value.asJsObject.fields
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("end-date").toString)
      val flow = fields("flow").convertTo[Array[Double]]
      val rainfall = fields("rainfall").convertTo[Array[Double]]
      val dwp = fields("dwp").convertTo[Array[Double]]
      val rdii = fields("rdii").convertTo[Array[Double]]
      GetResponse(startDate, endDate, flow, rainfall, dwp, rdii)
    }
  }

}
