package carldata.borsuk

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.RDIIApiObjects._
import carldata.borsuk.iandi.RDII
import carldata.borsuk.RDIIApiObjectsJsonProtocol._
import spray.json._

class RDIIApi {
  val models = collection.mutable.Map.empty[String, RDII]

  /**
    * Create new prediction model.
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
        //TODO: fit model
        complete(StatusCodes.OK)

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /** Fit the model to the training data */
  def list(modelId: String, params: ListRequest): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        //TODO: list model
        complete(StatusCodes.OK)

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

}
