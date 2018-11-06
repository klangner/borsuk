package carldata.borsuk.envelopes

import java.time.Duration

import ApiObjects._
import ApiObjectsJsonProtocol._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete
import carldata.series.Sessions.Session
import carldata.borsuk.helper.DateTimeHelper._

import scala.collection.mutable.Map
import spray.json._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


class EnvelopeApi {
  val models = Map.empty[String, Envelope]

  def create(params: CreateEnvelopeParams): StandardRoute = {
    if (models.contains(params.id)) {
      complete(StatusCodes.Conflict -> "Error: Model with this id already exist.")
    } else {
      models.put(params.id, new Envelope(params.modelType))
      complete(HttpResponse(StatusCodes.OK,
        entity = HttpEntity(MediaTypes.`application/json`, ModelCreatedResponse(params.id).toJson.compactPrint)))
    }
  }

  def fit(id: String, params: FitEnvelopeParams): StandardRoute = {
    models.get(id) match {
      case Some(model) =>
        Future {
          model.fit(params)
        }
        complete(StatusCodes.OK)
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
          complete(HttpResponse(StatusCodes.OK, entity = GetResponse(
            Seq(1.0, 2.0, 3.0),
            Seq(1.0, 2.0, 3.0),
            0.5,
            1.0,
            0.1,
            Seq(
              Session(dtToInstant(dateParse("2018-01-01T00:00:00")), dtToInstant(dateParse("2018-01-01T10:00:00"))),
              Session(dtToInstant(dateParse("2018-01-03T00:00:00")), dtToInstant(dateParse("2018-01-03T10:00:00"))),
              Session(dtToInstant(dateParse("2018-01-05T00:00:00")), dtToInstant(dateParse("2018-01-05T10:00:00")))
            )
          ).toJson.compactPrint))
        case None => complete(StatusCodes.NotFound)
      }
      case None => complete(StatusCodes.NotFound)
    }
  }

  def status(id: String): StandardRoute = {
    models.get(id) match {
      case Some(envelopeModel) => complete(HttpResponse(StatusCodes.OK,
        entity = ModelStatus(envelopeModel.buildNumber).toJson.compactPrint))
      case None => complete(StatusCodes.NotFound)
    }
  }

}
