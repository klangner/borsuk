package carldata.borsuk

import java.nio.file.Paths
import java.time.Duration

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import carldata.borsuk.envelope.ApiObjects.{CreateEnvelopeParams, FitEnvelopeParams}
import carldata.borsuk.envelope.ApiObjectsJsonProtocol._
import carldata.borsuk.envelope.EnvelopeResultHashMapJsonProtocol._
import carldata.borsuk.envelope.{Envelope, EnvelopeApi, EnvelopeResult}
import carldata.borsuk.helper.{Model, PVCHelper}
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
import scala.concurrent.duration._
import scala.language.postfixOps

class Routing() {
  val predictionApi = new PredictionAPI()
  val RDIIApi = new RdiiApi()
  val stormsApi = new StormsApi()
  val envelopeApi = new EnvelopeApi(RDIIApi)

  val settings: CorsSettings.Default = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))

  val MB = 1048576

  /** Loading models from Persistent Volume Claim */
  def load(): Unit = {
    def createEnvelope(model: Model): Option[Envelope] = {
      val envelope = new Envelope(model.modelType, model.id)
      envelope.model = model.content.parseJson.convertTo[immutable.HashMap[String, EnvelopeResult]]
      envelope.buildNumber += 1
      envelopeApi.models.put(model.id, envelope)
    }

    def createRdii(model: Model): Option[RDII] = {
      val rdii = new RDII(model.modelType, model.id)
      rdii.model = model.content.parseJson.convertTo[immutable.HashMap[String, RDIIObject]]
      rdii.buildNumber += 1
      RDIIApi.models.put(model.id, rdii)
    }

    def createStorm(model: Model): Option[Storms] = {
      val storm = new Storms(model.modelType, model.id)
      storm.model = model.content.parseJson.convertTo[immutable.HashMap[String, StormParams]]
      storm.buildNumber += 1
      stormsApi.models.put(model.id, storm)
    }

    val stormsPath = Paths.get("/borsuk_data/storms/")
    val rdiisPath = Paths.get("/borsuk_data/rdiis/")
    val envelopesPath = Paths.get("/borsuk_data/envelopes/")

    PVCHelper.loadModel(stormsPath, createStorm)
    PVCHelper.loadModel(rdiisPath, createRdii)
    PVCHelper.loadModel(envelopesPath, createEnvelope)
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
      withRequestTimeout(10.seconds) {
        withSizeLimit(20 * MB) {
          post {
            entity(as[FitRDIIParams])(data => RDIIApi.fit(id, data))
          }
        }
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
        withRequestTimeout(5.seconds) {
          withSizeLimit(20 * MB) {
            entity(as[FitStormsParams]) { data =>
              stormsApi.fit(id, data)
            }
          }
        }
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
      } ~ path("envelopes") {
        post {
          entity(as[CreateEnvelopeParams])(data => envelopeApi.create(data))
        }
      } ~ path("envelopes" / Segment) { id => {
        get {
          envelopeApi.status(id)
        }
      }
      } ~ path("envelopes" / Segment / "fit") { id => {
        post {
          withRequestTimeout(10.seconds) {
            withSizeLimit(20 * MB) {
              entity(as[FitEnvelopeParams])(data => envelopeApi.fit(id, data))
            }
          }
        }
      }
      } ~ path("envelopes" / Segment / "envelope") {
        id =>
          get {
            envelopeApi.list(id)
          }
      } ~ path("envelopes" / Segment / "envelope" / Segment) {
        (id, envelopeId) => {
          get {
            envelopeApi.get(id, envelopeId)
          }
        }
      }
    }
  }

