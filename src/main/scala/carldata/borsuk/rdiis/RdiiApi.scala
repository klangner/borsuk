package carldata.borsuk.rdiis

import java.time.Duration

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.rdiis.ApiObjects._
import carldata.borsuk.rdiis.ApiObjectsJsonProtocol._
import spray.json._

class RdiiApi {
  val models = collection.mutable.Map.empty[String, RDII]

  /**
    * Create new rdii model.
    * Use fit function to train this model
    */
  def create(params: CreateParams): StandardRoute = {
    if (models.contains(params.id)) {
      complete(StatusCodes.Conflict -> "Error: Model with this id already exist.")
    }
    else {
      val rdii = new RDII(params.modelType, params.id)
      models.put(params.id, rdii)
      val response = ModelCreatedResponse(params.id)

      complete(HttpResponse(
        StatusCodes.OK,
        entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
      ))
    }
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

  /** List the models of the training data */
  def list(modelId: String, sessionWindow: Duration): StandardRoute = {

    models.get(modelId) match {
      case Some(model) =>

        val response = ListResponse {
          model
            .list(sessionWindow)
            .map(x => ApiObjects.RDIIObject(x._1, x._2, x._3))
            .toArray
        }
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }

  /** Get the model of the training data */
  def get(modelId: String, rdiiId: String): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        model.get(rdiiId) match {
          case Some(rdii) =>
            val response: GetResponse = GetResponse(rdii._1, rdii._2, rdii._3, rdii._4, rdii._5, rdii._6)

            complete(HttpResponse(
              StatusCodes.OK,
              entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
            ))
          case None =>
            complete(StatusCodes.NotFound)
        }

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
