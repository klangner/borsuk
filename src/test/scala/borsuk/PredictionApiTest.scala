package borsuk

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.ApiObjects.{CreatePredictionParams, ModelCreatedResponse}
import carldata.borsuk.ApiObjectsJsonProtocol._
import carldata.borsuk.Routing
import org.scalatest.{Matchers, WordSpec}
import spray.json._


class PredictionApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  "The prediction" should {

    "create new model" in {
      val params = CreatePredictionParams("daily-pattern-v0")
      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/prediction",
        entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        responseAs[ModelCreatedResponse]
        status shouldEqual StatusCodes.OK
      }
    }
  }
}