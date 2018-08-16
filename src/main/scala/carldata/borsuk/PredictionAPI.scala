package carldata.borsuk

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.ApiObjects._
import carldata.borsuk.ApiObjectsJsonProtocol._
import carldata.borsuk.prediction.Prediction
import spray.json._


class PredictionAPI() {
  val models = collection.mutable.Map.empty[String, Prediction]

  /**
    * Create new prediction model.
    * Use fit function to train this model
    */
  def create(params: CreatePredictionParams): StandardRoute = {
    val prediction = new Prediction(params.modelType)
    models.put(prediction.id, prediction)
    val response = ModelCreatedResponse(prediction.id)

    complete(HttpResponse(
      StatusCodes.OK,
      entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
    ))
  }

  /** Fit the model to the training data */
  def fit(modelId: String, params: FitParams): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        model.fit(params.values)
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
        val values = model.predict()
        val response = PredictionResponse(values)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }


    if (models.contains(modelId)) {
      complete(s"""{"labels": [1,2,3]}""")
    }
    else complete(StatusCodes.NotFound)
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
