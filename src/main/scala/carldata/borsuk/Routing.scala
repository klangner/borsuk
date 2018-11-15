package carldata.borsuk

import java.nio.file.{Files, Paths}
import java.time.Duration

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import carldata.borsuk.prediction.ApiObjects.{CreatePredictionParams, FitPredictionParams}
import carldata.borsuk.prediction.ApiObjectsJsonProtocol._
import carldata.borsuk.prediction.PredictionAPI
import carldata.borsuk.rdiis.ApiObjects.{CreateParams, FitRDIIParams}
import carldata.borsuk.rdiis.ApiObjectsJsonProtocol.{CreateRDIIParamsFormat, FitRDIIParamsFormat}
import carldata.borsuk.rdiis.RDIIObjectHashMapJsonProtocol._
import carldata.borsuk.rdiis.{RDII, RDIIObject, RdiiApi}
import carldata.borsuk.storms.ApiObjects.{CreateStormsParams, FitStormsParams}
import carldata.borsuk.storms.ApiObjectsJsonProtocol.{CreateStormsParamsFormat, FitStormsParamsFormat}
import carldata.borsuk.storms.StormParamsHashMapJsonProtocol._
import carldata.borsuk.storms.Storms.StormParams
import carldata.borsuk.storms.{Storms, StormsApi}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.Seq
import scala.language.postfixOps

class Routing() {
  val predictionApi = new PredictionAPI()
  val RDIIApi = new RdiiApi()
  val stormsApi = new StormsApi()

  val settings: CorsSettings.Default = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))


  /** Loading models from Persistent Volume Claim */
  def load(): Unit = {

    //Storms
    val stormsPath = Paths.get("/borsuk_data/storms/")
    if (Files.exists(stormsPath)) {
      val stormVersions = Files.walk(stormsPath)
      for (stormVersion <- stormVersions.toArray) {
        for (stormModel <- Files.walk(Paths.get(stormVersion.toString)).toArray) {
          val stormModelPath = Paths.get(stormModel.toString)
          val content = new String(Files.readAllBytes(stormModelPath))
          val stormModelType = stormModelPath.getParent.getFileName.toString
          val stormModelId = stormModelPath.getFileName.toString
          val storm = new Storms(stormModelType, stormModelId)
          storm.model = content.parseJson.convertTo[immutable.HashMap[String, StormParams]]
          storm.buildNumber += 1
          stormsApi.models.put(stormModelId, storm)
        }
      }
    }

    //RDII
    val rdiisPath = Paths.get("/borsuk_data/rdiis/")
    if (Files.exists(rdiisPath)) {
      val rdiiVersions = Files.walk(rdiisPath)
      for (rdiiVersion <- rdiiVersions.toArray) {
        for (rdiiModel <- Files.walk(Paths.get(rdiiVersion.toString)).toArray) {
          val rdiiModelPath = Paths.get(rdiiModel.toString)
          val content = new String(Files.readAllBytes(rdiiModelPath))
          val rdiiModelType = rdiiModelPath.getParent.getFileName.toString
          val rdiiModelId = rdiiModelPath.getFileName.toString
          val rdii = new RDII(rdiiModelType, rdiiModelId)
          rdii.model = content.parseJson.convertTo[immutable.HashMap[String, RDIIObject]]
          rdii.buildNumber += 1
          RDIIApi.models.put(rdiiModelId, rdii)
        }
      }
    }
  }

  /** Routing */
  def route(): Route = cors(settings) {

    path("healthcheck") {
      complete("Ok")
    } ~ path("prediction") {
      post {
        entity(as[CreatePredictionParams])(params => predictionApi.create(params))
      }
    } ~ path("prediction" / Segment / "fit") { id =>
      post {
        entity(as[FitPredictionParams])(data => predictionApi.fit(id, data))
      }
    } ~ path("prediction" / Segment / "predict") { id =>
      post {
        entity(as[String])(data => predictionApi.predict(id, data))
      }
    } ~ path("prediction" / Segment) { id =>
      get {
        predictionApi.status(id)
      }
    } ~ path("rdiis") {
      post {
        entity(as[CreateParams])(params => RDIIApi.create(params))
      }
    } ~ path("rdiis" / Segment / "fit") { id =>
      post {
        entity(as[FitRDIIParams])(data => RDIIApi.fit(id, data))
      }
    } ~ (path("rdiis" / Segment / "rdii") & parameters("sessionWindow".as[String])) {
      (id, sessionWindow) =>
        get {
          RDIIApi.list(id, Duration.parse(sessionWindow))
        }
    } ~ path("rdiis" / Segment / "rdii" / Segment) {
      (modelId, rdiiId) =>
        get {
          RDIIApi.get(modelId, rdiiId)
        }
    } ~ path("rdiis" / Segment) { id =>
      get {
        RDIIApi.status(id)
      }
    } ~ path("storms") {
      post {
        entity(as[CreateStormsParams])(params => stormsApi.create(params))
      }
    } ~ path("storms" / Segment / "fit") { id =>
      post {
        entity(as[FitStormsParams])(data => stormsApi.fit(id, data))
      }
    } ~ (path("storms" / Segment / "storm") & parameters("sessionWindow".as[String])) {
      (id, sessionWindow) =>
        get {
          stormsApi.list(id, Duration.parse(sessionWindow))
        }
    } ~ path("storms" / Segment / "storm" / Segment) {
      (modelId, stormId) =>
        get {
          stormsApi.get(modelId, stormId)
        }
    } ~ path("storms" / Segment) { id =>
      get {
        stormsApi.status(id)
      }
    }
  }
}
