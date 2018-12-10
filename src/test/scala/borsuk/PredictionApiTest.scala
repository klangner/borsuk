package borsuk

import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.prediction.ApiObjects._
import carldata.borsuk.prediction.ApiObjectsJsonProtocol._
import carldata.borsuk.Routing
import carldata.borsuk.helper.TimeSeriesHelper
import carldata.series.Csv
import org.scalatest.{Matchers, WordSpec}
import spray.json._

import scala.io.Source


class PredictionApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  private val emptyTSP = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array())

  private val createModelRequest: HttpRequest = {
    val params = CreatePredictionParams("daily-pattern-v0", "secret-id")
    HttpRequest(
      HttpMethods.POST,
      uri = "/prediction",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The prediction" should {

    "create new model" in {
      createModelRequest ~> mainRoute() ~> check {
        val res = responseAs[ModelCreatedResponse]
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

    "not fit if model doesn't exist" in {
      val params = FitPredictionParams(emptyTSP, emptyTSP)
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
    "not fit the model with unsufficient data" in {
      val route = mainRoute()
      val trainData = 0.to(8).map(_ => 1.0).toArray
      val fitParams = FitPredictionParams(emptyTSP, emptyTSP)

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
            modelStatus.build shouldEqual 0
          }

        }
      }
    }


    "fit the model" in {
      val route = mainRoute()
      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = TimeSeriesHelper.toTimeSeriesParams(data.head)
      val rainfall = TimeSeriesHelper.toTimeSeriesParams(data(1))

      
      val fitParams = FitPredictionParams(flow, rainfall)

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