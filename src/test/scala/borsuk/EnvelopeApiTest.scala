package borsuk

import java.time.{LocalDateTime, Duration}

import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import carldata.borsuk.Routing
import org.scalatest.concurrent.Eventually
import carldata.borsuk.BasicApiObjects._
import carldata.borsuk.BasicApiObjectsJsonProtocol._
import carldata.borsuk.envelopes.ApiObjects._
import carldata.borsuk.envelopes.ApiObjectsJsonProtocol._

import scala.concurrent.duration._
import spray.json._

class EnvelopeApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport with Eventually {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  private def createEnvelopeModelRequest(modelType: String, id: String) =
    HttpRequest(HttpMethods.POST, uri = "/envelopes",
      entity = HttpEntity(MediaTypes.`application/json`,
        CreateEnvelopeParams(modelType, id).toJson.toString))

  private def fitEnvelopeRequest(id: String, fitEnvelopeParams: FitEnvelopeParams) = {
    HttpRequest(HttpMethods.POST,
      uri = s"/envelopes/${id}/fit",
      entity = HttpEntity(ContentTypes.`application/json`, fitEnvelopeParams.toJson.compactPrint)
    )
  }

  private def listEnvelopeRequest(id: String, sessionWindow: String) = {
    HttpRequest(HttpMethods.GET, uri = s"/envelopes/${id}/envelope?sessionWindow=${sessionWindow}")
  }

  private def checkEnvelopeModelStatus(id: String) = {
    HttpRequest(HttpMethods.GET, uri = s"/envelopes/${id}")
  }

  "The Envelope" should {

    "create new model" in {
      createEnvelopeModelRequest("test-model-type", "test-id") ~> mainRoute() ~> check {
        val resp = responseAs[ModelCreatedResponse]
        status shouldBe StatusCodes.OK
        resp.id shouldBe "test-id"
      }
    }

    "return conflict for model already created" in {
      val route = mainRoute()

      createEnvelopeModelRequest("test-model-type", "test-id") ~> route ~> check {
        status shouldBe StatusCodes.OK
        createEnvelopeModelRequest("test-model-type", "test-id") ~> route ~> check {
          status shouldBe StatusCodes.Conflict
          responseAs[String] shouldEqual "Error: Model with this id already exist."
        }
      }
    }


    "not fit if model does not exist" in {
      val route = mainRoute()
      val fitEnvelopeParams = FitEnvelopeParams(
        flow = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
        rainfall = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
        dryDayWindow = Duration.ofMinutes(5),
        stormIntensityWindow = Duration.ofMinutes(5),
        flowIntensityWindow = Duration.ofMinutes(5),
        minSessionWindow = Duration.ofMinutes(5),
        maxSessionWindow = Duration.ofMinutes(5)
      )

      fitEnvelopeRequest("test-id", fitEnvelopeParams) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not list Envelope if model does not exist" in {
      val route = mainRoute()
      listEnvelopeRequest("test-id", "PT15M") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not give status if model not found" in {
      checkEnvelopeModelStatus("test-id") ~> mainRoute() ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "fit the model" in {
      val route = mainRoute()
      createEnvelopeModelRequest("test-model-type", "test-id") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id"

        val fitEnvelopeParams = FitEnvelopeParams(
          flow = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
          rainfall = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
          dryDayWindow = Duration.ofMinutes(5),
          stormIntensityWindow = Duration.ofMinutes(5),
          flowIntensityWindow = Duration.ofMinutes(5),
          minSessionWindow = Duration.ofMinutes(5),
          maxSessionWindow = Duration.ofMinutes(5)
        )

        fitEnvelopeRequest("test-id", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(10 seconds), interval(2 seconds)) {
            checkEnvelopeModelStatus("test-id") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }
          }
        }

      }
    }

    "list the existing model" in {
    }

    "get the model" in {
      0 shouldBe 1
    }

    "not get the model when it does not exits" in {
      0 shouldBe 1
    }

  }

}
