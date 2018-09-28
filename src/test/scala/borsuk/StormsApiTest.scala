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

import scala.annotation.tailrec

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

  private def fitModelRequest(model: String, fitStormsParams: FitStormsParams): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = s"/storms/$model/fit",
      entity = HttpEntity(MediaTypes.`application/json`, fitStormsParams.toJson.compactPrint))
  }

  def checkStatus(id: String, route: Route): Int = {
    val getRequest = HttpRequest(HttpMethods.GET, uri = s"/storms/$id")
    getRequest ~> route ~> check {
      responseAs[ModelStormsStatus].build
    }
  }

  private def statusModelRequest(model: String): HttpRequest = {
    HttpRequest(HttpMethods.GET, uri = s"/storms/$model")
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
    "return new status after fit" in {
      val route = mainRoute()
      createModelRequest ~> route ~> check {
        val res = responseAs[ModelStormsCreatedResponse]

        val statusBeforeFit = checkStatus(res.id, route)

        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        fitModelRequest(res.id, fitParams) ~> route ~> check {

          @tailrec
          def cmpStatus(): Unit = {
            if (checkStatus(res.id, route) == statusBeforeFit) {
              Thread.sleep(5000)
              cmpStatus()
            }
          }

          cmpStatus()
          status shouldEqual StatusCodes.OK
          checkStatus(res.id, route) shouldEqual 1
        }
      }
    }
    "response 404 when fit without create" in {
      val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
        , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
      fitModelRequest("wrong_id", fitParams) ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }
  }
}
