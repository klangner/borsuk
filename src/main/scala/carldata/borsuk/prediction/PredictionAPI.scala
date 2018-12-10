package carldata.borsuk.prediction

import java.time.{Duration, LocalDate}
import java.time.temporal.{ChronoUnit, TemporalUnit}

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.borsuk.prediction.ApiObjects._
import carldata.borsuk.prediction.ApiObjectsJsonProtocol._
import spray.json._


class PredictionAPI() {
  val models = collection.mutable.Map.empty[String, Prediction]

  /**
    * Create new prediction model.
    * Use fit function to train this model
    */
  def create(params: CreatePredictionParams): StandardRoute = {
    if (models.contains(params.id)) {
      complete(StatusCodes.Conflict -> "Error: Model with this id already exist.")
    }
    else {
      val prediction = new Prediction(params.modelType, params.id)
      models.put(params.id, prediction)
      val response = ModelCreatedResponse(params.id)

      complete(HttpResponse(
        StatusCodes.OK,
        entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
      ))
    }
  }

  /** Fit the model to the training data */
  def fit(modelId: String, params: FitPredictionParams): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        model.fit(TimeSeriesHelper.parse(params.flow)
          , TimeSeriesHelper.parse(params.rainfall))
        complete(StatusCodes.OK)

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /**
    * Predict series values.
    * Model first should be trained with function fit
    */
  def predict(modelId: String, data: String): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        val values = model.predict(LocalDate.now().plus(Duration.ofDays(1)))
        val response = PredictionResponse(values)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /**
    * Check model status. This function can be used to check if new revision of the model is trained
    * and the current model metric score.
    */
  def status(modelId: String): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        val status = ModelStatus(model.buildNumber, model.score)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, status.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

}
