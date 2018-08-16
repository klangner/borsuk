package borsuk

import java.time.LocalDateTime

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.ApiObjects.{CreatePredictionParams, FitParams, ModelCreatedResponse, PredictionRequest}
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

    "not fit if model doesn't exist" in {
      val params = FitParams(LocalDateTime.now, Vector())
      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/prediction/000/fit",
        entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }
    "not predict if model doesn't exist" in {
      val params = PredictionRequest(LocalDateTime.now, 1)
      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/prediction/000/predict",
        entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not give status if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/prediction/000")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

  }
}