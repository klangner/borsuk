package carldata.borsuk.storms

import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.storms.ApiObjects._
import carldata.borsuk.storms.ApiObjectsJsonProtocol._
import spray.json._


class StormsApi {
  val models = collection.mutable.Map.empty[String, Storms]

  /**
    * Create new storms model.
    * Use fit function to train this model
    */
  def create(params: CreateStormsParams): StandardRoute = {
    val storm = new Storms(params.modelType)
    models.put(storm.id, storm)
    val response = ModelStormsCreatedResponse(storm.id)

    complete(HttpResponse(
      StatusCodes.OK,
      entity = HttpEntity(MediaTypes.`application/json`, response.toJson.compactPrint)
    ))
  }

  /** Fit the model to the training data */
  def fit(modelId: String, params: FitStormsParams): StandardRoute = {
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

        val response = ListStormsResponse {
          model
            .list()
            .map(x => StormsObject(x._1, instantToLDT(x._2.startIndex), instantToLDT(x._2.endIndex)))
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
  def get(modelId: String, stormId: String): StandardRoute = {
    models.get(modelId) match {
      case Some(model) =>
        val response: GetStormsResponse = model.get(stormId)
          .map(x => GetStormsResponse(instantToLDT(x._1), instantToLDT(x._2), x._3))
          .getOrElse(GetStormsResponse(LocalDateTime.now(), LocalDateTime.now(), Seq()))

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
        val status = ModelStormsStatus(model.buildNumber)
        complete(HttpResponse(
          StatusCodes.OK,
          entity = HttpEntity(MediaTypes.`application/json`, status.toJson.compactPrint)
        ))

      case None =>
        complete(StatusCodes.NotFound)
    }
  }
}
