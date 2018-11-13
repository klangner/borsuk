package carldata.borsuk.envelope

import java.time.Duration

import ApiObjects._
import ApiObjectsJsonProtocol._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete
import carldata.series.Sessions.Session
import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.rdiis.{RDII, RdiiApi}

import scala.collection.mutable.Map
import spray.json._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


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

  def list(id: String, sessionWindow: Duration): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) => complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`,
        ListResponse(envelope = envelopeModel.model.map(x => ApiObjects.EnvelopeObject(x._1, x._2.sessionWindow)).toArray)
          .toJson.compactPrint)))
      case None => complete(StatusCodes.NotFound)
    }
  }

  def get(id: String, envelopeId: String): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) => envelopeModel.model.get(envelopeId) match {
        case Some(singleEnvelope) =>
          complete(HttpResponse(StatusCodes.OK, entity = HttpEntity(ContentTypes.`application/json`,GetResponse(
            rainfall = Seq(1.0, 2.0, 3.0),
            flow = Seq(1.0, 2.0, 3.0),
            slope = 0.5,
            intercept = 1.0,
            rSquare =  0.1,
            dates = Seq(
              Session(dtToInstant(dateParse("2018-01-01T00:00:00")), dtToInstant(dateParse("2018-01-01T10:00:00"))),
              Session(dtToInstant(dateParse("2018-01-03T00:00:00")), dtToInstant(dateParse("2018-01-03T10:00:00"))),
              Session(dtToInstant(dateParse("2018-01-05T00:00:00")), dtToInstant(dateParse("2018-01-05T10:00:00")))
            )
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