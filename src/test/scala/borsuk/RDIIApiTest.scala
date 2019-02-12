package borsuk

import java.nio.file.Paths
import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.Routing
import carldata.borsuk.helper.{PVCHelper, TimeSeriesHelper}
import carldata.borsuk.rdiis.ApiObjects._
import carldata.borsuk.rdiis.ApiObjectsJsonProtocol._
import carldata.series.Csv
import org.scalatest.concurrent.Eventually
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpec}
import spray.json._

import scala.concurrent.duration._
import scala.io.Source


class RDIIApiTest extends WordSpec with Matchers
  with ScalatestRouteTest
  with SprayJsonSupport
  with Eventually
  with BeforeAndAfterAll {

  private val rdiisPath: String = "/borsuk_data/rdiis/"

  override def afterAll(): Unit = {
    //cleaning
    for (i <- 1 to 20) PVCHelper.deleteModel(Paths.get(rdiisPath + "rdii-v0"), "rdii-test-id" + i)
  }

  private def mainRoute()(implicit load: Boolean = false) = {
    val routing = new Routing()
    if (load) routing.load()
    routing.route()
  }

  private def createModelRequest(id: String): HttpRequest = {
    val params = CreateParams("rdii-v0", id)
    HttpRequest(
      HttpMethods.POST,
      uri = "/rdiis",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The RDII" should {

    "create new model" in {
      createModelRequest("rdii-test-id1") ~> mainRoute() ~> check {
        val res = responseAs[ModelCreatedResponse]
        res.id shouldEqual "rdii-test-id1"
        status shouldEqual StatusCodes.OK
      }
    }

    "create new model with id already in use" in {
      val route = mainRoute()
      createModelRequest("rdii-test-id2") ~> route ~> check {
        createModelRequest("rdii-test-id2")
      } ~> route ~> check {
        responseAs[String] shouldEqual "Error: Model with this id already exist."
        status shouldEqual StatusCodes.Conflict
      }
    }

    "not fit if model doesn't exist" in {
      val resolution: Duration = Duration.ofMinutes(10)
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofHours(12), Duration.ofHours(12), "rdii-v0")

      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/rdiis/000/fit",
        entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not list RDII if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/rdiis/000/rdii?sessionWindow=P2D")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not give status if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/rdiis/000")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fit the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofMinutes(10), Duration.ofMinutes(10), "rdii-v0")

      createModelRequest("rdii-test-id7") ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          eventually(timeout(20 seconds)) {
            val request = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}")

            request ~> route ~> check {
              val modelStatus = responseAs[ModelStatus]
              modelStatus.build shouldEqual 1
            }
          }
        }
      }
    }

    "list the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofDays(2), Duration.ofDays(2), "rdii-v0")

      createModelRequest("rdii-test-id8") ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}/rdii?sessionWindow=P2D")

          request ~> route ~> check {
            responseAs[ListResponse].rdii.length shouldEqual 0
          }
        }
      }
    }

    "try get model- not existed" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofDays(2), Duration.ofDays(2), "rdii-v0")

      createModelRequest("rdii-test-id9") ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val listRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}/rdii?sessionWindow=P2D")

          listRequest ~> route ~> check {
            responseAs[ListResponse].rdii.length shouldEqual 0
            val getRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}/rdii/test")

            getRequest ~> route ~> check {
              status shouldEqual StatusCodes.NotFound
            }
          }
        }
      }
    }

    "give status from loaded model" in {

      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofMinutes(10), Duration.ofMinutes(10), "rdii-v0")

      createModelRequest("rdii-test-id10") ~> route ~> check {
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/rdii-test-id10/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          eventually(timeout(20 seconds)) {
            val request = HttpRequest(HttpMethods.GET, uri = s"/rdiis/rdii-test-id10")

            request ~> route ~> check {
              val modelStatus = responseAs[ModelStatus]
              modelStatus.build shouldEqual 1
            }
          }
        }
      }

      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/rdiis/rdii-test-id10")

      request ~> mainRoute()(true) ~> check {
        val modelStatus = responseAs[ModelStatus]
        modelStatus.build shouldEqual 1
      }
    }

    "get model on real data" in {
      val route = mainRoute()
      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)
      val flowTSP = TimeSeriesHelper.toTimeSeriesParams(flow)
      val rainfallTSP = TimeSeriesHelper.toTimeSeriesParams(rainfall)


      val fitParams = FitRDIIParams(flowTSP, rainfallTSP, Duration.ofHours(12)
        , Duration.ofDays(2), Duration.ofDays(2), "rdii-v0")

      createModelRequest("rdii-test-id11") ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val listRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}/rdii?sessionWindow=P2D")
          eventually(timeout(120.seconds), interval(2.seconds)) {
            listRequest ~> route ~> check {
              val rdiis = responseAs[ListResponse].rdii
              rdiis.length should be > 0
              val json = response.entity.toString
              val getRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}/rdii/2382")
              //lets check date format (if dont have "Z" - which tell that is UTC zone)
              val singleEventStartDate = json.split(""""start-date":"""")(12).split(""""""").head
              val singleEventEndDate = json.split(""""end-date":"""")(12).split(""""""").head
              singleEventStartDate shouldBe "2014-01-26T09:20"
              singleEventEndDate shouldBe "2014-02-02T10:30"

              getRequest ~> route ~> check {
                status shouldEqual StatusCodes.OK
                val getResponse = responseAs[GetResponse]
                getResponse.flow.length should be > 0
                getResponse.rainfall.length should be > 0
                getResponse.rdii.length should be > 0
                getResponse.dwp.length should be > 0

              }
            }
          }
        }
      }
    }

    "fit the model twice" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val tsp = TimeSeriesParams(LocalDateTime.now, resolution, trainData)

      val fitParams = FitRDIIParams(tsp, tsp, Duration.ofHours(12)
        , Duration.ofMinutes(10), Duration.ofMinutes(10), "rdii-v0")

      createModelRequest("rdii-test-id12") ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/rdiis/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          eventually(timeout(20 seconds)) {
            val request = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}")

            request ~> route ~> check {
              val modelStatus = responseAs[ModelStatus]
              modelStatus.build shouldEqual 1

              fitRequest ~> route ~> check {
                status shouldEqual StatusCodes.OK
                eventually(timeout(20 seconds)) {
                  val request = HttpRequest(HttpMethods.GET, uri = s"/rdiis/${mcr.id}")

                  request ~> route ~> check {
                    val modelStatus2 = responseAs[ModelStatus]
                    modelStatus2.build shouldBe 2
                  }
                }
              }
            }
          }
        }
      }
    }

  }
}