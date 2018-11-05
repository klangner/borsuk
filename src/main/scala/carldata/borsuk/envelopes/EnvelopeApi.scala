package carldata.borsuk.envelopes

import java.time.Duration

import ApiObjects._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete


class EnvelopeApi {

  def create(params: CreateEnvelopeParams): StandardRoute = {
    complete("create ok" + params.id + " " + params.modelType)
  }

  def fit(id: String, params: FitEnvelopeParams): StandardRoute = {
    complete("fit ok - id " + id)
  }

  def list(id: String, sessionWindow: Duration): StandardRoute = {
    complete("list ok")
  }

  def get(id:String, envelopeId: String): StandardRoute = {
    complete("get ok, id - " + id + " envelopeId - " + envelopeId)
  }

  def status(id: String): StandardRoute = {
    complete("status ok, id - " + id)
  }

}
