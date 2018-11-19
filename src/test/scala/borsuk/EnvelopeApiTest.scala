package borsuk

import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects._
import carldata.borsuk.Routing
import carldata.borsuk.envelope.ApiObjects._
import carldata.borsuk.envelope.ApiObjectsJsonProtocol._
import carldata.series.Csv
import org.scalatest.concurrent.Eventually
import org.scalatest.{Matchers, WordSpec}
import spray.json._

import scala.concurrent.duration._
import scala.io.Source

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
      uri = s"/envelopes/$id/fit",
      entity = HttpEntity(ContentTypes.`application/json`, fitEnvelopeParams.toJson.compactPrint)
    )
  }

  private def listEnvelopeRequest(id: String) = {
    HttpRequest(HttpMethods.GET, uri = s"/envelopes/$id/envelope")
  }

  private def checkEnvelopeModelStatus(id: String) = {
    HttpRequest(HttpMethods.GET, uri = s"/envelopes/$id")
  }

  private def getEnvelopeModel(id: String, singleEnvelopeId: String) = {
    HttpRequest(HttpMethods.GET, uri = s"/envelopes/$id/envelope/$singleEnvelopeId")
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
      listEnvelopeRequest("test-id") ~> route ~> check {
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

          listEnvelopeRequest("test-id") ~> route ~> check {
            status shouldBe StatusCodes.OK
            responseAs[ListResponse].envelope.length shouldEqual 1
            //responseAs[ListResponse].envelope(0).id shouldBe "1"
            responseAs[ListResponse].envelope(0).sessionWindow shouldBe Duration.ofMinutes(5)
          }

        }
      }
    }

    "not get the envelope when model does not exits" in {
      val fakeModelId = "fakeModelId"
      val route = mainRoute()
      getEnvelopeModel("test-id", fakeModelId) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not get the envelope when model exists but wrong envelopeId is passed" in {
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

          getEnvelopeModel("test-id", "fakeModelId") ~> route ~> check {
            status shouldBe StatusCodes.NotFound
          }
        }
      }
    }

    "get the model" in {
      val route = mainRoute()

      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)

      createEnvelopeModelRequest("test-model-type", "test-id") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id"
        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), flow.values.toArray),
          TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), rainfall.values.toArray),
          dryDayWindow = Duration.ofMinutes(5),
          stormIntensityWindow = Duration.ofHours(6),
          flowIntensityWindow = Duration.ofMinutes(5),
          minSessionWindow = Duration.ofMinutes(715),
          maxSessionWindow = Duration.ofMinutes(725)
        )
        fitEnvelopeRequest("test-id", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(120 seconds), interval(2 seconds)) {
            checkEnvelopeModelStatus("test-id") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }

            listEnvelopeRequest("test-id") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ListResponse].envelope.length should be > 0

              val firstEnvelopeId = responseAs[ListResponse]
                .envelope
                .filter(x => x.sessionWindow.equals(Duration.ofHours(12)))
                .head.id

              getEnvelopeModel("test-id", firstEnvelopeId) ~> route ~> check {
                status shouldBe StatusCodes.OK
                responseAs[GetResponse].rainfall.take(5) shouldEqual Seq(42.75, 30.25, 28.5, 28.0, 24.5)
              }
            }
          }
        }
      }
    }


  }
}
