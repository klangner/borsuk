package carldata.borsuk.prediction

import java.time.LocalDateTime

import carldata.borsuk.helper.JsonHelper._
import spray.json._


/**
  * Here are definition of objects used in REST API with their json serialization
  */
object ApiObjects {

  case class CreatePredictionParams(modelType: String)

  case class ModelCreatedResponse(id: String)

  case class FitPredictionParams(startDate: LocalDateTime, values: Array[Double])

  case class PredictionRequest(startDate: LocalDateTime, samples: Int)

  case class PredictionResponse(values: Array[Double])

  case class ModelStatus(build: Int, score: Double)

}

/**
  * JSON serialization
  */
object ApiObjectsJsonProtocol extends DefaultJsonProtocol {

  import ApiObjects._

  /**
    * CreatePredictionParams formatter
    */
  implicit object CreatePredictionParamsFormat extends RootJsonFormat[CreatePredictionParams] {
    def write(params: CreatePredictionParams): JsObject = {
      JsObject(
        "type" -> JsString(params.modelType)
      )
    }

    def read(value: JsValue): CreatePredictionParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        CreatePredictionParams(modelType)
      case _ => CreatePredictionParams("daily-pattern-v0")
    }
  }

  /**
    * ModelCreatedResponse formatter
    */
  implicit object modelCreatedResponseJsonFormat extends RootJsonFormat[ModelCreatedResponse] {
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
  implicit object FitPredictionParamsFormat extends RootJsonFormat[FitPredictionParams] {
    def write(params: FitPredictionParams): JsObject = {
      JsObject(
        "start-date" -> JsString(params.startDate.toString),
        "values" -> JsArray(params.values.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): FitPredictionParams = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val values = request.get("values").map(arrayFromValue).getOrElse(Array.empty[Double])
        FitPredictionParams(startDate, values)
      case _ => FitPredictionParams(LocalDateTime.now(), Array.empty)
    }
  }

  /**
    * ModelStatus formatter
    */
  implicit object ModelStatusFormat extends RootJsonFormat[ModelStatus] {
    def write(status: ModelStatus): JsObject = {
      JsObject(
        "build" -> JsNumber(status.build),
        "score" -> JsNumber(status.score)
      )
    }

    def read(value: JsValue): ModelStatus = value match {
      case JsObject(request) =>
        val build = request.get("build").map(doubleFromValue).map(_.toInt).getOrElse(0)
        val score = request.get("score").map(doubleFromValue).getOrElse(0.0)
        ModelStatus(build, score)
      case _ => ModelStatus(0, 0.0)
    }
  }

  /**
    * PredictionRequest formatter
    */
  implicit object PredictionRequestFormat extends RootJsonFormat[PredictionRequest] {
    def write(req: PredictionRequest): JsObject = {
      JsObject(
        "start-date" -> JsString(req.startDate.toString),
        "samples" -> JsNumber(req.samples)
      )
    }

    def read(value: JsValue): PredictionRequest = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        val samples = request.get("samples").map(doubleFromValue).map(_.toInt).getOrElse(0)
        PredictionRequest(startDate, samples)
      case _ => PredictionRequest(LocalDateTime.now(), 0)
    }
  }

  /**
    * PredictionResponse formatter
    */
  implicit object PredictionResponseFormat extends RootJsonFormat[PredictionResponse] {
    def write(res: PredictionResponse): JsObject = {
      JsObject(
        "values" -> JsArray(res.values.map(JsNumber(_)).toVector)
      )
    }

    def read(value: JsValue): PredictionResponse = value match {
      case JsObject(request) =>
        val values = request.get("values").map(arrayFromValue).getOrElse(Array.empty[Double])
        PredictionResponse(values)
      case _ => PredictionResponse(Array.empty)
    }
  }

}
