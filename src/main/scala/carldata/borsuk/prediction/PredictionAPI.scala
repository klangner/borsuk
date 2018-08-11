package carldata.borsuk.prediction

import java.util.UUID.randomUUID

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute



class PredictionAPI() {
  val models = collection.mutable.Map.empty[String, String]

  /**
    * Create new prediction model.
    * Use fit function to train this model
    */
  def create(): StandardRoute = {
    val id = randomUUID().toString
    models.put(id, id)
    complete(s"""{"id": "$id"}""")
  }

  /** Fit the model to the training data */
  def fit(modelId: String, data: String): StandardRoute = {
    if (models.contains(modelId)) {
      complete(StatusCodes.OK)
    }
    else complete(StatusCodes.NotFound)
  }

  /**
    * Predict series values.
    * Model first should be trained with function fit
    */
  def predict(modelId: String, data: String): StandardRoute = {
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
    if (models.contains(modelId)) {
      complete(s"""{"build": "1", "score": 0.78}""")
    }
    else complete(StatusCodes.NotFound)
  }

}
