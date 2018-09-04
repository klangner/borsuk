package borsuk

import java.time.LocalDateTime

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.RDIIApiObjects._
import carldata.borsuk.RDIIApiObjectsJsonProtocol._
import carldata.borsuk.Routing
import org.scalatest.{Matchers, WordSpec}
import spray.json._


class RDIIApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  private val createModelRequest: HttpRequest = {
    val params = CreateRDIIParams("rdii-v0")
    HttpRequest(
      HttpMethods.POST,
      uri = "/rdii",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The RDII" should {

    "create new model" in {
      createModelRequest ~> mainRoute() ~> check {
        responseAs[ModelCreatedResponse]
        status shouldEqual StatusCodes.OK
      }
    }

    "not fit if model doesn't exist" in {
      val params = FitRDIIParams(LocalDateTime.now, Array(), Array(), Array())
      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/rdii/000/fit",
        entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not list RDII if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/rdii/000/list?startDate=2018-01-02&endDate=2018-01-02&stormSessionWindows=60&stormIntensityWindow=120&dryDayWindow=120")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not give status if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/rdii/000")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fit the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val windowData = 1.to(4).map(x => x * 60).toArray
      val fitParams = FitRDIIParams(LocalDateTime.now, trainData, trainData, windowData)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/rdii/${mcr.id}")

          request ~> route ~> check {
            val modelStatus = responseAs[ModelStatus]
            modelStatus.build shouldEqual 1
          }

        }
      }
    }

    "list the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val windowData = 1.to(4).map(x => x * 60).toArray
      val fitParams = FitRDIIParams(LocalDateTime.now, trainData, trainData, windowData)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/rdii/${mcr.id}/list?startDate=2018-01-02" +
            s"&endDate=2018-01-05&stormSessionWindows=60" +
            s"&stormIntensityWindow=120&dryDayWindow=160")

          request ~> route ~> check {
            responseAs[ListResponse].rdii.length shouldEqual 0
          }

        }
      }
    }


  }
}