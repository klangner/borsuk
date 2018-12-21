package borsuk

import java.nio.file.Paths
import java.time.{Duration, LocalDateTime}

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import carldata.borsuk.BasicApiObjects._
import carldata.borsuk.Routing
import carldata.borsuk.envelope.ApiObjects._
import carldata.borsuk.envelope.ApiObjectsJsonProtocol._
import carldata.borsuk.helper.{DateTimeHelper, PVCHelper}
import carldata.series.Csv
import org.scalatest._
import org.scalatest.concurrent.Eventually
import spray.json._

import scala.concurrent.duration._
import scala.io.Source

class EnvelopeApiTest extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with SprayJsonSupport
  with Eventually
  with BeforeAndAfter
  with BeforeAndAfterAll {

  private val envelopesPath: String = "/borsuk_data/envelopes/"

  override def afterAll(): Unit = {
    //cleaning
    for (i <- 1 to 20) PVCHelper.deleteModel(Paths.get(envelopesPath + "envelope-v0"), "test-id" + i)
  }

  private def mainRoute(): Route = {
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
      createEnvelopeModelRequest("envelope-v0", "test-id1") ~> mainRoute() ~> check {
        val resp = responseAs[ModelCreatedResponse]
        status shouldBe StatusCodes.OK
        resp.id shouldBe "test-id1"
      }

    }

    "return conflict for model already created" in {
      val route = mainRoute()

      createEnvelopeModelRequest("envelope-v0", "test-id2") ~> route ~> check {
        status shouldBe StatusCodes.OK
        createEnvelopeModelRequest("envelope-v0", "test-id2") ~> route ~> check {
          status shouldBe StatusCodes.Conflict
          responseAs[String] shouldEqual "Error: Model with this id already exist."
        }
      }
    }


    "not fit if model does not exist" in {
      val route = mainRoute()
      val fitEnvelopeParams = FitEnvelopeParams(
        flow = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
        , rainfall = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
        , dryDayWindow = Duration.ofMinutes(5)
        , stormIntensityWindow = Duration.ofMinutes(5)
        , flowIntensityWindow = Duration.ofMinutes(5)
        , minSessionWindow = Duration.ofMinutes(5)
        , maxSessionWindow = Duration.ofMinutes(5)
        , 3.0
        , "envelope-v0"
      )

      fitEnvelopeRequest("test-id3", fitEnvelopeParams) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not list Envelope if model does not exist" in {
      val route = mainRoute()
      listEnvelopeRequest("test-id4") ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not give status if model not found" in {
      checkEnvelopeModelStatus("test-id5") ~> mainRoute() ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "fit the model" in {
      val route = mainRoute()
      createEnvelopeModelRequest("envelope-v0", "test-id6") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id6"

        val fitEnvelopeParams = FitEnvelopeParams(
          flow = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
          , rainfall = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
          , dryDayWindow = Duration.ofMinutes(5)
          , stormIntensityWindow = Duration.ofMinutes(5)
          , flowIntensityWindow = Duration.ofMinutes(5)
          , minSessionWindow = Duration.ofMinutes(5)
          , maxSessionWindow = Duration.ofMinutes(5)
          , 3.0
          , "envelope-v0"
        )

        fitEnvelopeRequest("test-id6", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(10.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id6") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }
          }
        }

      }
    }

    "list the existing model" in {
      val route = mainRoute()

      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)

      createEnvelopeModelRequest("envelope-v0", "test-id7") ~> route ~> check {

        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id7"
        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), flow.values.toArray)
          , TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), rainfall.values.toArray)
          , dryDayWindow = Duration.ofDays(2)
          , stormIntensityWindow = Duration.ofHours(6)
          , flowIntensityWindow = Duration.ofHours(1)
          , minSessionWindow = Duration.ofMinutes(720)
          , maxSessionWindow = Duration.ofMinutes(720)
          , 3.0
          , "envelope-v0"
        )
        fitEnvelopeRequest("test-id7", fitEnvelopeParams) ~> route ~> check {

          status shouldBe StatusCodes.OK
          eventually(timeout(120.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id7") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }

            listEnvelopeRequest("test-id7") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ListResponse].envelope.length should be > 0
            }
          }
        }
      }

    }

    "not get the envelope when model does not exits" in {
      val fakeModelId = "fakeModelId"
      val route = mainRoute()
      getEnvelopeModel("test-id8", fakeModelId) ~> route ~> check {
        status shouldBe StatusCodes.NotFound
      }
    }

    "not get the envelope when model exists but wrong envelopeId is passed" in {
      val route = mainRoute()

      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)

      createEnvelopeModelRequest("envelope-v0", "test-id9") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id9"

        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), flow.values.toArray)
          , TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), rainfall.values.toArray)
          , dryDayWindow = Duration.ofDays(2)
          , stormIntensityWindow = Duration.ofHours(6)
          , flowIntensityWindow = Duration.ofHours(1)
          , minSessionWindow = Duration.ofMinutes(720)
          , maxSessionWindow = Duration.ofMinutes(720)
          , 3.0
          , "envelope-v0"
        )

        fitEnvelopeRequest("test-id9", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK

          eventually(timeout(120.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id9") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }
          }

          getEnvelopeModel("test-id9", "fakeModelId") ~> route ~> check {
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

      createEnvelopeModelRequest("envelope-v0", "test-id10") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id10"
        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), flow.values.toArray)
          , TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), rainfall.values.toArray)
          , dryDayWindow = Duration.ofDays(2)
          , stormIntensityWindow = Duration.ofHours(6)
          , flowIntensityWindow = Duration.ofHours(1)
          , minSessionWindow = Duration.ofMinutes(720)
          , maxSessionWindow = Duration.ofMinutes(720)
          , 3.0
          , "envelope-v0"
        )
        fitEnvelopeRequest("test-id10", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(120.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id10") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }

            listEnvelopeRequest("test-id10") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ListResponse].envelope.length should be > 0

              val envelopes = responseAs[ListResponse]
                .envelope

              envelopes.map(_.id).contains("0") shouldBe true

              val firstEnvelopeId = envelopes
                .filter(x => x.sessionWindow.equals(Duration.ofHours(12)))
                .head.id

              getEnvelopeModel("test-id10", firstEnvelopeId) ~> route ~> check {
                status shouldBe StatusCodes.OK
                val json = response.entity.toString

                //lets check date format (if dont have "Z" - which tell that is UTC zone)
                val singleEventStartDate = json.split(""""from":"""")(4).split(""""""").head
                val singleEventEndDate = json.split(""""to":"""")(4).split(""""""").head
                singleEventStartDate shouldBe "2014-03-08T10:10:00"
                singleEventEndDate shouldBe "2014-03-09T08:35:00"

                val getResponse = responseAs[GetResponse]
                getResponse.rainfall.take(5) shouldEqual Seq(42.75, 30.25, 28.5, 28.0, 24.5)
                getResponse.flow.take(5)
                  .zip(Seq(8.092, 5.389, 4.226, 11.138, 3.939))
                  .forall(x => Math.abs(x._2 - x._1) < 0.001) shouldBe true
              }

            }
          }
        }
      }
    }

    "create rdiis" in {
      val route = mainRoute()

      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)

      createEnvelopeModelRequest("envelope-v0", "test-id11") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id11"
        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), flow.values.toArray)
          , TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), rainfall.values.toArray)
          , dryDayWindow = Duration.ofDays(2)
          , stormIntensityWindow = Duration.ofHours(6)
          , flowIntensityWindow = Duration.ofHours(1)
          , minSessionWindow = Duration.ofMinutes(720)
          , maxSessionWindow = Duration.ofMinutes(720)
          , 3.0
          , "envelope-v0"
        )
        fitEnvelopeRequest("test-id11", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(120.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id11") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }

            listEnvelopeRequest("test-id11") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ListResponse].envelope.length should be > 0

              val rdiiListRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/test-id11/rdii?sessionWindow=PT12H")

              rdiiListRequest ~> route ~> check {
                import carldata.borsuk.rdiis.ApiObjectsJsonProtocol._

                responseAs[carldata.borsuk.rdiis.ApiObjects.ListResponse].rdii.length should be > 0
              }
            }
          }
        }
      }
    }


    "produce same events in rdiis and storms" in {
      val route = mainRoute()

      val csv = Source.fromResource("copley-pump.csv").getLines().mkString("\n")
      val data = Csv.fromString(csv)
      val flow = data.head
      val rainfall = data(1)

      createEnvelopeModelRequest("envelope-v0", "test-id12") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id12"
        val fitEnvelopeParams = FitEnvelopeParams(
          TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), flow.values.toArray)
          , TimeSeriesParams(DateTimeHelper.dateParse("2013-10-22T11:55:00Z"), Duration.ofMinutes(5), rainfall.values.toArray)
          , dryDayWindow = Duration.ofDays(2)
          , stormIntensityWindow = Duration.ofHours(6)
          , flowIntensityWindow = Duration.ofHours(1)
          , minSessionWindow = Duration.ofMinutes(720)
          , maxSessionWindow = Duration.ofMinutes(720)
          , 3.0
          , "envelope-v0"
        )

        fitEnvelopeRequest("test-id12", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(120.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id12") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ModelStatus].build shouldBe 1
            }

            listEnvelopeRequest("test-id12") ~> route ~> check {
              status shouldBe StatusCodes.OK
              responseAs[ListResponse].envelope.length should be > 0

              val firstEnvelopeId = responseAs[ListResponse]
                .envelope
                .filter(x => x.sessionWindow.equals(Duration.ofHours(12)))
                .head.id

              getEnvelopeModel("test-id12", firstEnvelopeId) ~> route ~> check {
                status shouldBe StatusCodes.OK
                val getResponse = responseAs[GetResponse]
                val storms = getResponse.dates

                val rdiiListRequest = HttpRequest(HttpMethods.GET, uri = s"/rdiis/test-id12/rdii?sessionWindow=PT12H")

                rdiiListRequest ~> route ~> check {
                  import carldata.borsuk.rdiis.ApiObjectsJsonProtocol._

                  val rdiis = responseAs[carldata.borsuk.rdiis.ApiObjects.ListResponse].rdii

                  storms.flatMap {
                    storm =>
                      rdiis.find(rdii => DateTimeHelper.dtToInstant(rdii.startDate) == storm.startIndex
                        && DateTimeHelper.dtToInstant(rdii.endDate) == storm.endIndex
                      )
                  }.length shouldEqual storms.length
                }

              }


            }
          }
        }
      }
    }

    "fit the model twice" in {
      val route = mainRoute()
      val route2 = mainRoute()
      createEnvelopeModelRequest("envelope-v0", "test-id13") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[ModelCreatedResponse].id shouldBe "test-id13"

        val fitEnvelopeParams = FitEnvelopeParams(
          flow = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
          , rainfall = TimeSeriesParams(LocalDateTime.now(), Duration.ofMinutes(5), Array(1.0, 2.0, 3.0))
          , dryDayWindow = Duration.ofMinutes(5)
          , stormIntensityWindow = Duration.ofMinutes(5)
          , flowIntensityWindow = Duration.ofMinutes(5)
          , minSessionWindow = Duration.ofMinutes(5)
          , maxSessionWindow = Duration.ofMinutes(5)
          , 3.0
          , "envelope-v0"
        )

        fitEnvelopeRequest("test-id13", fitEnvelopeParams) ~> route ~> check {
          status shouldBe StatusCodes.OK
          eventually(timeout(10.seconds), interval(2.seconds)) {
            checkEnvelopeModelStatus("test-id13") ~> route ~> check {
              status shouldBe StatusCodes.OK
              val modelStatus1 = responseAs[ModelStatus]
              modelStatus1.build shouldBe 1
              fitEnvelopeRequest("test-id13", fitEnvelopeParams) ~> route2 ~> check {
                status shouldBe StatusCodes.OK
                eventually(timeout(10.seconds), interval(2.seconds)) {
                  checkEnvelopeModelStatus("test-id13") ~> route2 ~> check {
                    status shouldBe StatusCodes.OK
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
