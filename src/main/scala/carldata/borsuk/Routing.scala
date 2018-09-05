package carldata.borsuk

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import carldata.borsuk.ApiObjects.{CreatePredictionParams, FitPredictionParams}
import carldata.borsuk.ApiObjectsJsonProtocol._
import carldata.borsuk.autoii.RDIIApiObjects.CreateRDIIParams
import carldata.borsuk.autoii.RDIIApiObjectsJsonProtocol.{CreateRDIIParamsFormat, FitParamsFormat}
import carldata.borsuk.helper.DateTimeHelper
import carldata.borsuk.autoii.{RDIIApi, RDIIApiObjects}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq
import scala.language.postfixOps

class Routing() {
  val predictionApi = new PredictionAPI()
  val rdiiApi = new RDIIApi()

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
        entity(as[CreateRDIIParams])(params => rdiiApi.create(params))
      }
    } ~ path("autoii" / Segment / "fit") { id =>
      post {
        entity(as[RDIIApiObjects.FitRDIIParams])(data => rdiiApi.fit(id, data))
      }
    } ~ (path("autoii" / Segment / "rdii") & parameters("startDate".as[String], "endDate".as[String]
      , "stormSessionWindows".as[Int], "stormIntensityWindow".as[Int], "dryDayWindow".as[Int])) {
      (id, startDate, endDate, stormSessionWindows, stormIntensityWindow, dryDayWindow) =>
        get {
          val params = RDIIApiObjects.ListRequest(DateTimeHelper.dateParse(startDate), DateTimeHelper.dateParse(endDate)
            , stormSessionWindows, stormIntensityWindow, dryDayWindow)
          rdiiApi.list(id, params)
        }
    } ~ path("autoii" / Segment / "rdii" / Segment) {
      (modelId, rdiiId) =>
        get {
          rdiiApi.get(modelId, rdiiId)
        }
    } ~ path("autoii" / Segment) { id =>
      get {
        rdiiApi.status(id)
      }
    }
  }

}
