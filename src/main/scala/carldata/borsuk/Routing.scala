package carldata.borsuk

import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import carldata.borsuk.ApiObjects.CreatePredictionParams
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq
import scala.language.postfixOps


class Routing() {

  val predictionApi = new PredictionAPI()

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
      post{
        entity(as[CreatePredictionParams])(params => predictionApi.create(params))
      }
    } ~ path("prediction" / Segment / "fit") { id =>
      post {
        entity(as[String])( data => predictionApi.fit(id, data) )
      }
    } ~ path("prediction" / Segment / "predict") { id =>
      post {
        entity(as[String]) ( data => predictionApi.predict(id, data) )
      }
    } ~ path("prediction" / Segment ) { id =>
      get {
        predictionApi.status(id)
      }
    }
  }

}
