package carldata.borsuk.envelope

import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.borsuk.envelope.ApiObjects._
import carldata.borsuk.envelope.ApiObjectsJsonProtocol._
import carldata.borsuk.rdiis.{RDII, RdiiApi}
import spray.json._

import scala.collection.mutable.Map
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


class EnvelopeApi(rdiiApi :RdiiApi) {
  val models = Map.empty[String, Envelope]

  /**
    * Create new envelope and rdiis corresponded to it
    */
  def create(params: CreateEnvelopeParams): StandardRoute = {
    if (models.contains(params.id)) {
      complete(StatusCodes.Conflict -> "Error: Model with this id already exist.")
    } else {
      models.put(params.id, new Envelope(params.modelType))

      val rdii = new RDII(params.modelType, params.id)
      rdiiApi.models.put(params.id, rdii)


      complete(HttpResponse(StatusCodes.OK,
        entity = HttpEntity(ContentTypes.`application/json`, ModelCreatedResponse(params.id).toJson.compactPrint)))
    }
  }

  def fit(id: String, params: FitEnvelopeParams): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) =>
        rdiiApi.models.get(id) match {
          case Some(rdiiModel) =>
            Future {
              envelopeModel.fit(params, rdiiModel)
            }
            complete(StatusCodes.OK)
          case None => complete(StatusCodes.NotFound)
        }
      case None => complete(StatusCodes.NotFound)
    }
  }

  def list(id: String): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel: Envelope) =>

        complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`,
        ListResponse(envelopeModel.list.map(x => ApiObjects.EnvelopeObject(x._1, x._2.sessionWindow)).toArray)
          .toJson.compactPrint)))
      case None => complete(StatusCodes.NotFound)
    }
  }

  def get(id: String, envelopeId: String): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) => envelopeModel.model.get(envelopeId) match {
        case Some(singleEnvelope: EnvelopeResult) =>
          complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`,GetResponse(
            singleEnvelope.rainfall,
            singleEnvelope.flows,
            singleEnvelope.slope,
            singleEnvelope.intercept,
            singleEnvelope.rSquare,
            singleEnvelope.dates
          ).toJson.compactPrint)))

        case None => complete(StatusCodes.NotFound)
      }
      case None => complete(StatusCodes.NotFound)
    }
  }

  def status(id: String): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) => complete(HttpResponse(StatusCodes.OK,
        entity = HttpEntity(ContentTypes.`application/json`, ModelStatus(envelopeModel.buildNumber).toJson.compactPrint)))
      case None => complete(StatusCodes.NotFound)
    }
  }

}
