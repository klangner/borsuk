package carldata.borsuk.autoii

import java.time.{Duration, LocalDateTime}

import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.helper.JsonHelper._
import spray.json._

/**
  * Here are definition of objects used in REST API with their json serialization
  */
object ApiObjects {

  case class CreateParams(modelType: String)

  case class ModelCreatedResponse(id: String)

  case class FitAutoIIParams(startDate: LocalDateTime, resolution: Duration, flow: Array[Double], rainfall: Array[Double]
                             , window: Array[Int], stormSessionWindows: Duration, stormIntensityWindow: Duration
                             , dryDayWindow: Duration)

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
        "type" -> JsString(params.modelType)
      )
    }

    def read(value: JsValue): CreateParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        CreateParams(modelType)
      case _ => CreateParams("daily-pattern-v0")
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
  implicit object FitAutoIIParamsFormat extends RootJsonFormat[FitAutoIIParams] {
    def write(params: FitAutoIIParams): JsObject = {
      JsObject(
        "start-date" -> JsString(params.startDate.toString),
        "resolution" -> JsString(params.resolution.toString),
        "stormSessionWindows" -> JsString(params.stormSessionWindows.toString),
        "stormIntensityWindow" -> JsString(params.stormIntensityWindow.toString),
        "dryDayWindow" -> JsString(params.dryDayWindow.toString),
        "flow" -> JsArray(params.flow.map(JsNumber(_)).toVector),
        "rainfall" -> JsArray(params.rainfall.map(JsNumber(_)).toVector),
        "window" -> JsArray(params.window.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): FitAutoIIParams = value match {
      case JsObject(request) =>
        println("res try \t"+request.get("resolution").map(stringFromValue).map(Duration.parse))
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val resolution = request.get("resolution").map(stringFromValue).map(Duration.parse).getOrElse(Duration.ofHours(1))
        val flow = request.get("flow").map(arrayFromValue).getOrElse(Array.empty[Double])
        val rainfall = request.get("rainfall").map(arrayFromValue).getOrElse(Array.empty[Double])
        val window = request.get("window").map(arrayFromValue).map(_.map(_.toInt)).getOrElse(Array.empty[Int])
        val stormSessionWindows: Duration = request.get("stormSessionWindows").map(stringFromValue).map(Duration.parse)
          .getOrElse(Duration.ofHours(1))
        val stormIntensityWindow: Duration = request.get("stormIntensityWindow").map(stringFromValue).map(Duration.parse)
          .getOrElse(Duration.ofHours(1))
        val dryDayWindow = request.get("dryDayWindow").map(stringFromValue).map(Duration.parse).getOrElse(Duration.ofHours(1))

        FitAutoIIParams(startDate, resolution, flow, rainfall, window, stormSessionWindows, stormIntensityWindow, dryDayWindow)
      case _ => FitAutoIIParams(LocalDateTime.now(), Duration.ofHours(1), Array.empty, Array.empty, Array.empty, Duration.ofHours(1), Duration.ofHours(1), Duration.ofHours(1))
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
      val startDate: LocalDateTime = DateTimeHelper.dateParse(fields("start-date").toString)
      val endDate: LocalDateTime = DateTimeHelper.dateParse(fields("end-date").toString)
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
            a.convertTo[ApiObjects.RDIIObject]
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
