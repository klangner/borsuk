package borsuk

import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
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

  private val modelId = "secret-id"
  private val createModelRequest: HttpRequest = {
    val params = CreateStormsParams("storms-v0", "secret-id")
    HttpRequest(
      HttpMethods.POST,
      uri = "/storms",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  private def fitModelRequest(model: String, fitStormsParams: FitStormsParams): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = s"/storms/$model/fit",
      entity = HttpEntity(MediaTypes.`application/json`, fitStormsParams.toJson.compactPrint))
  }

  private def listModelRequest(model: String, sessionWindows: String): HttpRequest = {
    HttpRequest(HttpMethods.GET, uri = s"/storms/$model/storm?sessionWindow=$sessionWindows")
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
        status shouldEqual StatusCodes.Conflict
      }
    }

    "list all storms" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        fitModelRequest(modelId, fitParams)
      } ~> route ~> check {
        listModelRequest(modelId, "PT5M")
      } ~> route ~> check {
        val stormsCount = responseAs[ListStormsResponse].storms.length
        stormsCount shouldEqual 4
      }
    }

    "list joined storms" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        fitModelRequest(modelId, fitParams)
      } ~> route ~> check {
        listModelRequest(modelId, "PT7M")
      } ~> route ~> check {
        val stormsCount = responseAs[ListStormsResponse].storms.length
        stormsCount shouldEqual 3
      }
    }

    "list joined to one storm" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        fitModelRequest(modelId, fitParams)
      } ~> route ~> check {
        listModelRequest(modelId, "PT20M")
      } ~> route ~> check {
        val stormsCount = responseAs[ListStormsResponse].storms.length
        stormsCount shouldEqual 1
      }
    }

    "should't get list of storms when model not exist" in {
      val route = mainRoute()

      listModelRequest("none", "PT5M") ~> route ~> check {
        responseAs[String] shouldEqual "Error: Model with this id doesn't exist."
        status shouldEqual StatusCodes.NotFound
      }
    }
  }
}
