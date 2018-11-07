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

  private val createEnvelopeModelRequest =
    HttpRequest(HttpMethods.POST, uri = "/envelopes",
      entity = HttpEntity(MediaTypes.`application/json`,
        CreateEnvelopeParams("test-model-type", "test-id").toJson.toString))

  private def fitEnvelopeRequest(id: String) = {
    HttpRequest(HttpMethods.POST,
      uri = s"/envelopes/${id}/fit",
      entity = HttpEntity(ContentTypes.`application/json`, FitEnvelopeParams(
        TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
        TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0)),
        Duration.ofMinutes(5),
        Duration.ofMinutes(5),
        Duration.ofMinutes(5),
        Duration.ofMinutes(5),
        Duration.ofMinutes(5)
      ).toJson.compactPrint))
  }

  "The Envelope" should {

    "create new model" in {
      createEnvelopeModelRequest ~> mainRoute() ~> check {
        val resp = responseAs[ModelCreatedResponse]
        status shouldBe StatusCodes.OK
        resp.id shouldBe "test-id"
      }
    }

    "return conflict for model already created" in {
      val route = mainRoute()

      createEnvelopeModelRequest ~> route ~> check {
        status shouldBe StatusCodes.OK
        createEnvelopeModelRequest ~> route ~> check {
          status shouldBe StatusCodes.Conflict
          responseAs[String] shouldEqual "Error: Model with this id already exist."
        }
      }
    }


    "not fit if model does not exist" in {
      val route = mainRoute()

      fitEnvelopeRequest("test-id") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not list Envelope if model does not exist" in {
      0 shouldBe 1
    }

    "not give status if model not found" in {
      0 shouldBe 1

    }

    "fit the model" in {
      0 shouldBe 1
    }

    "list the existing model" in {
      0 shouldBe 1
    }

    "get the model" in {
      0 shouldBe 1
    }

    "not get the model when it does not exits" in {
      0 shouldBe 1
    }

  }

}
