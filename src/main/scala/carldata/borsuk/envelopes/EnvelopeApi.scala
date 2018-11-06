package carldata.borsuk.envelopes

import java.time.Duration

import ApiObjects._
import ApiObjectsJsonProtocol._
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, MediaTypes, StatusCodes}
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete
import carldata.series.Sessions.Session
import carldata.borsuk.helper.DateTimeHelper._
import spray.json._


class EnvelopeApi {

  def create(params: CreateEnvelopeParams): StandardRoute = {
    complete(HttpResponse(StatusCodes.OK,
      entity = HttpEntity(MediaTypes.`application/json`, ModelCreatedResponse(params.id).toJson.compactPrint)))
  }

  def fit(id: String, params: FitEnvelopeParams): StandardRoute = {
    complete(HttpResponse(StatusCodes.OK))
  }

  def list(id: String, sessionWindow: Duration): StandardRoute = {
    complete(HttpResponse(StatusCodes.OK,
      entity = ListResponse(Array(ApiObjects.EnvelopeObject("fakeId", sessionWindow))).toJson.compactPrint))
  }

  def get(id: String, envelopeId: String): StandardRoute = {

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
  }

  def status(id: String): StandardRoute = {
    complete(HttpResponse(StatusCodes.OK, entity = ModelStatus(0).toJson.compactPrint))
  }

}
