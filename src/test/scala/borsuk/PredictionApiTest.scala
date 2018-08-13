package borsuk

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.Routing
import org.scalatest.{Matchers, WordSpec}


class PredictionApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  "The prediction" should {

    "create new model" in {
      Post("/prediction") ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.OK
      }
    }
  }
}