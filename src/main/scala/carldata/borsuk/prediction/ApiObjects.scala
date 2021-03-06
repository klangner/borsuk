package carldata.borsuk.prediction

import java.time.{Duration, LocalDateTime}
import java.util.UUID.randomUUID

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.helper.JsonHelper._
import spray.json._


/**
  * Here are definition of objects used in REST API with their json serialization
  */
object ApiObjects {

  case class CreatePredictionParams(modelType: String, id: String)

  case class ModelCreatedResponse(id: String)

  case class FitPredictionParams(flow: TimeSeriesParams, rainfall: TimeSeriesParams)

  case class PredictionRequest(startDate: LocalDateTime)

  case class PredictionResponse(flow: TimeSeriesParams)

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
        "type" -> JsString(params.modelType),
        "id" -> JsString(params.id)
      )
    }

    def read(value: JsValue): CreatePredictionParams = value match {
      case JsObject(request) =>
        val modelType = request.get("type").map(stringFromValue).getOrElse("daily-pattern-v0")
        val id = request.get("id").map(stringFromValue).getOrElse(randomUUID().toString)
        CreatePredictionParams(modelType, id)
      case _ => CreatePredictionParams("daily-pattern-v0", randomUUID().toString)
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
    val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())

    def write(params: FitPredictionParams): JsObject = {
      JsObject(
        "flow" -> params.flow.toJson,
        "rainfall" -> params.rainfall.toJson
      )
    }

    def read(value: JsValue): FitPredictionParams = value match {
      case JsObject(request) =>

        val flow = request.get("flow").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
        val rainfall = request.get("rainfall").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
        FitPredictionParams(flow, rainfall)
      case _ => FitPredictionParams(emptyTSP, emptyTSP)
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
        "start-date" -> JsString(req.startDate.toString)
      )
    }

    def read(value: JsValue): PredictionRequest = value match {
      case JsObject(request) =>
        val startDate = request.get("start-date").map(timestampFromValue).getOrElse(LocalDateTime.now())
        PredictionRequest(startDate)
      case _ => PredictionRequest(LocalDateTime.now())
    }
  }

  /**
    * PredictionResponse formatter
    */
  implicit object PredictionResponseFormat extends RootJsonFormat[PredictionResponse] {
    val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())
    def write(res: PredictionResponse): JsObject = {
      JsObject(
        "flow" -> res.flow.toJson
      )
    }

    def read(value: JsValue): PredictionResponse = value match {
      case JsObject(request) =>
        val flow = request.get("flow").map(_.convertTo[TimeSeriesParams]).getOrElse(emptyTSP)
        PredictionResponse(flow)
      case _ => PredictionResponse(emptyTSP)
    }
  }

}
