package borsuk

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.Routing
import carldata.borsuk.storms.ApiObjects._
import carldata.borsuk.storms.ApiObjectsJsonProtocol._
import org.scalatest.{Matchers, WordSpec}
import spray.json._

class StormsApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {
  private def mainRoute(): Route = {
    val routing = new Routing()
    routing.route()
  }

  private val createModelRequest: HttpRequest = {
    val params = CreateStormsParams("storms-v0", "secret-id")
    HttpRequest(
      HttpMethods.POST,
      uri = "/storms",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The storms" should {

    "create new model" in {
      createModelRequest ~> mainRoute() ~> check {
        val res = responseAs[ModelStormsCreatedResponse]
        res.id shouldEqual "secret-id"
        status shouldEqual StatusCodes.OK
      }
    }
    "create new model with id already in use" in {
      val route = mainRoute()
      createModelRequest ~> route ~> check {
        createModelRequest
      } ~> route ~> check {
        responseAs[String] shouldEqual "Error: Model with this id already exist."
      }
    }
  }

}
