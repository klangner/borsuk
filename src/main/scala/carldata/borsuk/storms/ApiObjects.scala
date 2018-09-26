package carldata.borsuk.storms

import java.time.{Duration, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.helper.JsonHelper._
import spray.json._

/**
  * Here are definition of objects used in REST API with their json serialization
  */
object ApiObjects {

  case class CreateStormsParams(modelType: String, id: String)

  case class ModelStormsCreatedResponse(id: String)

  case class FitStormsParams(startDate: LocalDateTime, resolution: Duration, rainfall: Array[Double])

  case class StormsObject(id: String, startDate: LocalDateTime, endDate: LocalDateTime)

  case class ListStormsResponse(storms: Array[StormsObject])

  case class GetStormsResponse(startDate: LocalDateTime, endDate: LocalDateTime, values: Seq[Double])

  case class ModelStormsStatus(build: Int)

}

/**
  * JSON serialization
  */
object ApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import ApiObjects._

  /**
    * CreateStormsParams formatter
    */
  implicit object CreateStormsParamsFormat extends RootJsonFormat[CreateStormsParams] {
    def write(params: CreateStormsParams): JsObject = {
      JsObject(
        "type" -> JsString(params.modelType),
        "id" -> JsString(params.id)
      )
    }

    def read(value: JsValue): CreateStormsParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("storms-v0")
        val id = request.get("id").map(stringFromValue).getOrElse(randomUUID().toString)
        CreateStormsParams(modelType, id)
      case _ => CreateStormsParams("storms-v0", randomUUID().toString)
    }
  }

  /**
    * ModelCreatedResponse formatter
    */
  implicit object ModelCreatedResponseJsonFormat extends RootJsonFormat[ModelStormsCreatedResponse] {
    def write(response: ModelStormsCreatedResponse): JsObject = {
      JsObject(
        "id" -> JsString(response.id)
      )
    }

    def read(value: JsValue): ModelStormsCreatedResponse = value match {
      case JsObject(response) =>
        val id = response.get("id").map(stringFromValue).getOrElse("")
        ModelStormsCreatedResponse(id)
      case _ => ModelStormsCreatedResponse("")
    }
  }

  /**
    * FitParams formatter
    */
  implicit object FitStormsParamsFormat extends RootJsonFormat[FitStormsParams] {
    def write(params: FitStormsParams): JsObject = {
      JsObject(
        "start-date" -> JsString(params.startDate.toString),
        "resolution" -> JsString(params.resolution.toString),
        "rainfall" -> JsArray(params.rainfall.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): FitStormsParams = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val resolution = request.get("resolution").map(stringFromValue).map(Duration.parse).getOrElse(Duration.ofHours(1))
        val rainfall = request.get("rainfall").map(arrayFromValue).getOrElse(Array.empty[Double])

        FitStormsParams(startDate, resolution, rainfall)
      case _ => FitStormsParams(LocalDateTime.now(), Duration.ofHours(1), Array.empty)
    }
  }

  /**
    * ModelStatus formatter
    */
  implicit object ModelStatusFormat extends RootJsonFormat[ModelStormsStatus] {
    def write(status: ModelStormsStatus): JsObject = {
      JsObject(
        "build" -> JsNumber(status.build)
      )
    }

    def read(value: JsValue): ModelStormsStatus = value match {
      case JsObject(request) =>
        val build = request.get("build").map(doubleFromValue).map(_.toInt).getOrElse(0)
        ModelStormsStatus(build)
      case _ => ModelStormsStatus(0)
    }
  }

  /**
    * StormsObject formatter
    */
  implicit object StormsObjectFormat extends RootJsonFormat[ApiObjects.StormsObject] {
    def write(storm: ApiObjects.StormsObject): JsObject = {
      JsObject(
        "id" -> storm.id.toJson,
        "start-date" -> storm.startDate.toString.toJson,
        "end-date" -> storm.endDate.toString.toJson
      )
    }

    def read(json: JsValue): ApiObjects.StormsObject = {
      val fields = json.asJsObject.fields
      val id: String = fields("id").toString
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("end-date").toString)
      StormsObject(id, startDate, endDate)
    }
  }

  /**
    * Model List formatter
    */
  implicit object ListResponseFormat extends RootJsonFormat[ListStormsResponse] {
    def write(status: ListStormsResponse): JsObject = {
      JsObject(
        "storms" -> JsArray(status.storms.map {
          _.toJson
        }.toVector)
      )
    }

    def read(value: JsValue): ListStormsResponse = {
      value.asJsObject().fields("storms") match {
        case JsArray(arr) =>
          ListStormsResponse(arr.map { a =>
            a.convertTo[StormsObject]
          }.toArray)
        case _ => ListStormsResponse(Array())
      }
    }
  }

  /**
    * Model Get formatter
    */
  implicit object GetResponseFormat extends RootJsonFormat[GetStormsResponse] {
    def write(response: GetStormsResponse): JsObject = {
      JsObject(
        "start-date" -> JsString(response.startDate.toString),
        "end-date" -> JsString(response.endDate.toString),
        "values" -> JsArray(response.values.map({
          _.toJson
        }).toVector))
    }

    def read(value: JsValue): GetStormsResponse = {
      val fields = value.asJsObject.fields
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      fields("values") match {
        case JsArray(arr) =>
          val vs = arr.map { a =>
            a.convertTo[Double]
          }.toArray
          GetStormsResponse(startDate, endDate, vs)
        case _ => GetStormsResponse(startDate, endDate, Seq())
      }

    }
  }

}