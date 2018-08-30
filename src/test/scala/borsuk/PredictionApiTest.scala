package borsuk

import java.time.LocalDateTime

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.PredictionApiObjects._
import carldata.borsuk.ApiObjectsJsonProtocol._
import carldata.borsuk.Routing
import org.scalatest.{Matchers, WordSpec}
import spray.json._


class PredictionApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  private val createModelRequest: HttpRequest = {
    val params = CreatePredictionParams("daily-pattern-v0")
    HttpRequest(
      HttpMethods.POST,
      uri = "/prediction",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The prediction" should {

    "create new model" in {
      createModelRequest ~> mainRoute() ~> check {
        responseAs[ModelCreatedResponse]
        status shouldEqual StatusCodes.OK
      }
    }

    "not fit if model doesn't exist" in {
      val params = FitParams(LocalDateTime.now, Array())
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

    "fit the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val fitParams = FitParams(LocalDateTime.now, trainData)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/prediction/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/prediction/${mcr.id}")

          request ~> route ~> check {
            val modelStatus = responseAs[ModelStatus]
            modelStatus.build shouldEqual 1
          }

        }
      }
    }

  }
}