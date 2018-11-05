package carldata.borsuk.envelopes

import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.server.Directives.complete

class EnvelopeApi {

  def create(): StandardRoute = {
    complete("create ok")
  }

  def fit(): StandardRoute = {
    complete("fit ok")
  }

  def list(): StandardRoute = {
    complete("list ok")
  }

  def get(): StandardRoute = {
    complete("get ok")
  }

  def status() : StandardRoute = {
    complete("status ok")
  }

}
