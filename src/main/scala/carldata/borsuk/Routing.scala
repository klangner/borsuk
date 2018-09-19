package carldata.borsuk

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import carldata.borsuk.prediction.ApiObjects.{CreatePredictionParams, FitPredictionParams}
import carldata.borsuk.prediction.ApiObjectsJsonProtocol._
import carldata.borsuk.autoii.ApiObjects.{CreateParams, FitAutoIIParams}
import carldata.borsuk.autoii.ApiObjectsJsonProtocol.{CreateRDIIParamsFormat, FitAutoIIParamsFormat}
import carldata.borsuk.autoii.AutoIIApi
import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.prediction.PredictionAPI
import carldata.borsuk.storms.StormsApi
import carldata.borsuk.storms.ApiObjects.{CreateStormsParams, FitStormsParams}
import carldata.borsuk.storms.ApiObjectsJsonProtocol.{CreateStormsParamsFormat, FitStormsParamsFormat}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq
import scala.language.postfixOps

class Routing() {
  val predictionApi = new PredictionAPI()
  val autoIIApi = new AutoIIApi()
  val stormsApi = new StormsApi()

  val settings: CorsSettings.Default = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))


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
    } ~ path("autoii") {
      post {
        entity(as[CreateParams])(params => autoIIApi.create(params))
      }
    } ~ path("autoii" / Segment / "fit") { id =>
      post {
        entity(as[FitAutoIIParams])(data => autoIIApi.fit(id, data))
      }
    } ~ (path("autoii" / Segment / "rdii") & parameters("startDate".as[String], "endDate".as[String])) {
      (id, startDate, endDate) =>
        get {
          autoIIApi.list(id, DateTimeHelper.dateParse(startDate), DateTimeHelper.dateParse(endDate))
        }
    } ~ path("autoii" / Segment / "rdii" / Segment) {
      (modelId, rdiiId) =>
        get {
          autoIIApi.get(modelId, rdiiId)
        }
    } ~ path("autoii" / Segment) { id =>
      get {
        autoIIApi.status(id)
      }
    } ~ path("storms") {
      post {
        entity(as[CreateStormsParams])(params => stormsApi.create(params))
      }
    } ~ path("storms" / Segment / "fit") { id =>
      post {
        entity(as[FitStormsParams])(data => stormsApi.fit(id, data))
      }
    } ~ (path("storms" / Segment / "storm")) {
      (id) =>
        get {
          stormsApi.list(id)
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
