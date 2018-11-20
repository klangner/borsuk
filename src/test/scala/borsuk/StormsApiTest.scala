package borsuk

import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.Routing
import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.storms.ApiObjects._
import carldata.borsuk.storms.ApiObjectsJsonProtocol._
import carldata.series.Csv
import org.scalatest.concurrent.Eventually
import org.scalatest.{Matchers, WordSpec}
import spray.json._

import scala.concurrent.duration._
import scala.io.Source

class StormsApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport with Eventually {
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

  def checkStatus(id: String, route: Route): Int = {
    val getRequest = HttpRequest(HttpMethods.GET, uri = s"/storms/$id")
    getRequest ~> route ~> check {
      responseAs[ModelStormsStatus].build
    }
  }

  private def statusModelRequest(model: String): HttpRequest = {
    HttpRequest(HttpMethods.GET, uri = s"/storms/$model")
  }

  private def listModelRequest(model: String, sessionWindows: String): HttpRequest = {
    HttpRequest(HttpMethods.GET, uri = s"/storms/$model/storm?sessionWindow=$sessionWindows")
  }

  private def getModelRequest(model: String, stormId: String): HttpRequest = {
    HttpRequest(HttpMethods.GET, uri = s"/storms/$model/storm/$stormId")
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

    "response 404 when fit without create" in {
      val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
        , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
      fitModelRequest("wrong_id", fitParams) ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "list all storms" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!

        fitModelRequest(modelId, fitParams) ~> route ~> check {
          eventually(timeout(20.seconds), interval(2.seconds)) {
            listModelRequest(modelId, "PT5M") ~> route ~> check {
              val stormsCount = responseAs[ListStormsResponse].storms.length
              stormsCount shouldEqual 2
            }
          }
        }
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
        eventually(timeout(20.seconds)) {
          listModelRequest(modelId, "PT10M") ~> route ~> check {
            val stormsCount = responseAs[ListStormsResponse].storms.length
            stormsCount shouldEqual 3
          }
        }
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
        eventually(timeout(10.seconds)) {
          listModelRequest(modelId, "PT20M") ~> route ~> check {
            val stormsCount = responseAs[ListStormsResponse].storms.length
            stormsCount shouldEqual 1
          }
        }
      }
    }

    "return properly model id" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        fitModelRequest(modelId, fitParams)
      } ~> route ~> check {
        eventually(timeout(10.seconds)) {
          listModelRequest(modelId, "PT20M") ~> route ~> check {
            val id: String = responseAs[ListStormsResponse]
              .storms.head
              .id.replaceAll("\"", "")

            id shouldEqual "4"
          }
        }
      }
    }

    "list storms two times" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        eventually(timeout(10.seconds)) {
          fitModelRequest(modelId, fitParams) ~> route ~> check {
            listModelRequest(modelId, "PT10M")
          } ~> route ~> check {
            val stormsCount = responseAs[ListStormsResponse].storms.length
            stormsCount shouldEqual 3
            listModelRequest(modelId, "PT20M")
          } ~> route ~> check {
            val stormsCount = responseAs[ListStormsResponse].storms.length
            stormsCount shouldEqual 1
          }
        }
      }
    }

    "should't get list of storms when model not exist" in {
      val route = mainRoute()

      listModelRequest("none", "PT5M") ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "get storm" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array(0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0)))
        //will create 4 sessions, lets get first and check the values!
        fitModelRequest(modelId, fitParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(20.seconds)) {
            listModelRequest(modelId, "PT7M") ~> route ~> check {
              val stormId = responseAs[ListStormsResponse].storms.head.id
              getModelRequest(modelId, stormId)
            } ~> route ~> check {
              responseAs[GetStormsResponse].values shouldEqual Array(1.0)
              status shouldEqual StatusCodes.OK
            }
          }
        }
      }
    }

    "should't get storm when model not exist" in {
      val route = mainRoute()

      getModelRequest("none", "none") ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "give nothing when storm not exist" in {
      val route = mainRoute()

      createModelRequest ~> route ~> check {
        val fitParams = FitStormsParams(TimeSeriesParams(LocalDateTime.now, Duration.ofMinutes(5)
          , Array()))
        fitModelRequest(modelId, fitParams)
      } ~> route ~> check {
        listModelRequest(modelId, "PT7M")
      } ~> route ~> check {
        getModelRequest(modelId, "none")
      } ~> route ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fit for copley-pump(46225 samples) should fit in less than 20 seconds" in {
      val route = mainRoute()
      val copleyPumpTs = Csv.fromString(Source.fromResource("copley-pump.csv").getLines().mkString("\n"))
      val startDate = DateTimeHelper.instantToLDT(copleyPumpTs(1).index.head)
      val rainfall = copleyPumpTs(1).values.toArray[Double]

      val fitParams = FitStormsParams(TimeSeriesParams(startDate, Duration.ofMinutes(5), rainfall))
      createModelRequest ~> route ~> check {
        val res = responseAs[ModelStormsCreatedResponse]
        val statusBeforeFit = checkStatus(res.id, route)

        fitModelRequest(res.id, fitParams) ~> route ~> check {
          eventually(timeout(20.seconds)) {
            val statusAfterFit = checkStatus(res.id, route)
            statusBeforeFit should not equal statusAfterFit
            status shouldEqual StatusCodes.OK
            checkStatus(res.id, route) shouldEqual 1
          }
        }
      }
    }

  }
}
