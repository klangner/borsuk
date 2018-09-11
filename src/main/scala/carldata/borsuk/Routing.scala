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
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq
import scala.language.postfixOps

class Routing() {
  val predictionApi = new PredictionAPI()
  val autoIIApi = new AutoIIApi()

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
    } ~ (path("autoii" / Segment / "rdii") & parameters("startDate".as[String], "endDate".as[String]
      , "stormSessionWindows".as[Int], "stormIntensityWindow".as[Int], "dryDayWindow".as[Int])) {
      (id, startDate, endDate, stormSessionWindows, stormIntensityWindow, dryDayWindow) =>
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
    }
  }

}
