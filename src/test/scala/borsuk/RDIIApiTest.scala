package borsuk

import java.time.{Duration, LocalDateTime}
import java.util.UUID.randomUUID

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshal
import carldata.borsuk.Routing
import carldata.borsuk.autoii.{ApiObjects, RDII}
import carldata.borsuk.autoii.ApiObjects._
import carldata.borsuk.autoii.ApiObjectsJsonProtocol._
import carldata.series.Csv
import org.scalatest.{Matchers, WordSpec}
import spray.json._

import scala.io.Source


class RDIIApiTest extends WordSpec with Matchers with ScalatestRouteTest with SprayJsonSupport {

  private def mainRoute() = {
    val routing = new Routing()
    routing.route()
  }

  private val createModelRequest: HttpRequest = {
    val params = CreateParams("rdii-v0")
    HttpRequest(
      HttpMethods.POST,
      uri = "/autoii",
      entity = HttpEntity(MediaTypes.`application/json`, params.toJson.compactPrint))
  }

  "The RDII" should {
    "its alive!" in {
      var models = collection.mutable.Map.empty[String, String]
      val id = randomUUID().toString
      models.put(id, "kotel")

      println("result" + models.get(id))


    }
    "create new model" in {
      createModelRequest ~> mainRoute() ~> check {
        responseAs[ModelCreatedResponse]
        status shouldEqual StatusCodes.OK
      }
    }

    "not fit if model doesn't exist" in {
      val resolution: Duration = Duration.ofMinutes(10)
      val stormSessionWindows: Duration = Duration.ofHours(1)
      val stormIntensityWindow: Duration = Duration.ofHours(1)
      val dryDayWindow = Duration.ofHours(1)

      val fitParams = FitAutoIIParams(LocalDateTime.now, resolution, Array(), Array(), Array()
        , stormSessionWindows, stormIntensityWindow, dryDayWindow)

      val request = HttpRequest(
        HttpMethods.POST,
        uri = "/autoii/000/fit",
        entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not list RDII if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/autoii/000/rdii?startDate=2018-01-02&endDate=2018-01-02&stormSessionWindows=60&stormIntensityWindow=120&dryDayWindow=120")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "not give status if model doesn't exist" in {
      val request = HttpRequest(
        HttpMethods.GET,
        uri = "/autoii/000")

      request ~> mainRoute() ~> check {
        status shouldEqual StatusCodes.NotFound
      }
    }

    "fit the model" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val windowData = 1.to(4).map(x => x * 60).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val stormSessionWindows: Duration = Duration.ofHours(1)
      val stormIntensityWindow: Duration = Duration.ofHours(1)
      val dryDayWindow = Duration.ofHours(1)

      val fitParams = FitAutoIIParams(LocalDateTime.now, resolution, trainData, trainData, windowData
        , stormSessionWindows, stormIntensityWindow, dryDayWindow)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/autoii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}")

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
      val resolution: Duration = Duration.ofMinutes(10)
      val stormSessionWindows: Duration = Duration.ofHours(1)
      val stormIntensityWindow: Duration = Duration.ofHours(1)
      val dryDayWindow = Duration.ofHours(1)

      val fitParams = FitAutoIIParams(LocalDateTime.now, resolution, trainData, trainData, windowData
        , stormSessionWindows, stormIntensityWindow, dryDayWindow)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/autoii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val request = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}/rdii?startDate=2018-01-02" +
            s"&endDate=2018-01-05&stormSessionWindows=60" +
            s"&stormIntensityWindow=120&dryDayWindow=160")

          request ~> route ~> check {
            responseAs[ListResponse].rdii.length shouldEqual 0
          }

        }
      }
    }
    "try get model- not existed" in {
      val route = mainRoute()
      val trainData = 0.to(1000).map(_ => 1.0).toArray
      val windowData = 1.to(4).map(x => x * 60).toArray
      val resolution: Duration = Duration.ofMinutes(10)
      val stormSessionWindows: Duration = Duration.ofHours(1)
      val stormIntensityWindow: Duration = Duration.ofHours(1)
      val dryDayWindow = Duration.ofHours(1)

      val fitParams = FitAutoIIParams(LocalDateTime.now, resolution, trainData, trainData, windowData
        , stormSessionWindows, stormIntensityWindow, dryDayWindow)

      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/autoii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          val listRequest = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}/rdii?startDate=2018-01-02" +
            s"&endDate=2018-01-05")

          listRequest ~> route ~> check {
            responseAs[ListResponse].rdii.length shouldEqual 0
            val getRequest = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}/rdii/test)")

            getRequest ~> route ~> check {
              responseAs[GetResponse].flow shouldEqual Array()
            }
          }

        }
      }
    }

    "run with real data" in {
      val route = mainRoute()

      def checkStatus(id: String): Int = {
        val getRequest = HttpRequest(HttpMethods.GET, uri = s"/autoii/$id")

        getRequest ~> route ~> check {
          responseAs[ApiObjects.ModelStatus].build
        }
      }

      val rain = Source.fromResource("rain1.txt").getLines().mkString("\n").split("\n").map(_.toDouble).toArray
      val flow = Source.fromResource("flow1.txt").getLines().mkString("\n").split("\n").map(_.toDouble).toArray

      println("here we are:\t"+Source.fromResource("flow1.txt").getLines().mkString("\n").split("\n").map(_.toDouble).take(5))
      Source.fromResource("flow1.txt").getLines().mkString("\n").split("\n").map(_.toDouble).take(5).map(println)
      Source.fromResource("rain1.txt").getLines().mkString("\n").split("\n").map(_.toDouble).take(5).map(println)
      val windowData = Array(60,1440)
      val resolution: Duration = Duration.ofMinutes(5)
      val stormSessionWindows: Duration = Duration.ofHours(12)
      val stormIntensityWindow: Duration = Duration.ofHours(6)
      val dryDayWindow = Duration.ofHours(48)
        println(flow.length + "\t"+ Source.fromResource("flow1.txt").getLines().mkString("\n").length)
      println(rain.length+ "\t"+ Source.fromResource("rain1.txt").getLines().mkString("\n").length)
      val fitParams = FitAutoIIParams(LocalDateTime.now, resolution, flow, rain, windowData
        , stormSessionWindows, stormIntensityWindow, dryDayWindow)
      println(createModelRequest.toString())
      createModelRequest ~> route ~> check {
        val mcr = responseAs[ModelCreatedResponse]
        println(mcr)
        val statusBefore = checkStatus(mcr.id)
        println(statusBefore)
        val fitRequest = HttpRequest(
          HttpMethods.POST,
          uri = s"/autoii/${mcr.id}/fit",
          entity = HttpEntity(MediaTypes.`application/json`, fitParams.toJson.compactPrint))

        fitRequest ~> route ~> check {
          status shouldEqual StatusCodes.OK
          println(status)
          //          Thread.sleep(20000)
          val listRequest = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}/rdii?startDate=2018-01-02" +
            s"&endDate=2018-01-05")

          listRequest ~> route ~> check {
            println(listRequest)

            def cmpStatus(): Unit = {
              println(checkStatus(mcr.id) +"\t"+ statusBefore)
              if (checkStatus(mcr.id) == statusBefore) {

                Thread.sleep(5000)
                cmpStatus()
              }
            }
          cmpStatus()
            responseAs[ListResponse].rdii.map(println)
            responseAs[ListResponse].rdii.length shouldEqual 0
            val getRequest = HttpRequest(HttpMethods.GET, uri = s"/autoii/${mcr.id}/rdii/test)")

            getRequest ~> route ~> check {
              println(getRequest)
              responseAs[GetResponse].flow.map(println)
              responseAs[GetResponse].flow shouldEqual Array()
            }
          }

        }
      }
    }


  }
}