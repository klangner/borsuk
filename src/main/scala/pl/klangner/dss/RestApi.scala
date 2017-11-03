package pl.klangner.dss

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import org.slf4j.LoggerFactory


class RestApi() {

  private val Log = LoggerFactory.getLogger(getClass.getName)

  def ping(str: String): StandardRoute = {
    complete("pong: " + str)
  }

}