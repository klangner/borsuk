package borsuk

import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import carldata.borsuk.Routing
import org.scalatest.concurrent.Eventually
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

    }

    "not list Envelope if model does not exist" in {

    }

    "not give status if model not found" in {

    }

    "fit the model" in {

    }

    "list the existing model" in {


    }

    "get the model" in {

    }

    "not get the model when it does not exits" in {


    }

  }

}
