package carldata.borsuk

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.RDIIApiObjects._
import carldata.borsuk.RDIIApiObjectsJsonProtocol._
import carldata.borsuk.iandi.RDII
import spray.json._

class RDIIApi {
  val models = collection.mutable.Map.empty[String, RDII]

  /**
    * Create new rdii model.
    * Use fit function to train this model
    */
  def create(params: CreateRDIIParams): StandardRoute = {
    val rdii = new RDII(params.modelType)
    models.put(rdii.id, rdii)
    val response = ModelCreatedResponse(rdii.id)

    complete(HttpResponse(
      StatusCodes.OK,
      entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
    ))

  }

  /** Fit the model to the training data */
  def fit(modelId: String, params: FitRDIIParams): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        model.fit(params)
        complete(StatusCodes.OK)

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /** Fit the model to the training data */
  def list(modelId: String, params: ListRequest): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        val response = ListResponse(Array())

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
        val status = ModelStatus(model.buildNumber)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, status.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

}
