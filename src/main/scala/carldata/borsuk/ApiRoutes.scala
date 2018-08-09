package carldata.borsuk

import java.time.Instant

import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import carldata.series.{Csv, TimeSeries}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq
import scala.language.postfixOps

case class Parameters(project: String, flow: String, projectsUrl: String)

class ApiRoutes(storage: Storage) {
  val modelApi = new ModelAPI()

  val settings: CorsSettings.Default = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))


  /** Routing */
  def route(): Route = cors(settings) {
    path("api" / "healthcheck") {
      complete("Ok")
    } ~ path("api" / "model") {
      post {
        entity(as[String]) {
          body =>
            modelApi.create(body)
        }
      }
    } ~ path("api" / "model" / Segment / "fit") {
      id =>
        post {
          entity(as[String]) {
            body =>
              modelApi.fit(id, body)
          }
        }
    } ~ path("api" / "model" / Segment / "predict") {
      id =>
        post {
          entity(as[String]) {
            body =>
              modelApi.predict(id, body)
          }
        }
    } ~ path("api" / "model" / Segment / "status") {
      id =>
        post {
          modelApi.status(id)
        }
    }
  }


  def anomaly(ts: TimeSeries[Double]): StandardRoute = {
    val cleanedTs = Anomaly.fixAnomalies(ts)
    complete(Csv.toCsv(cleanedTs))
  }

  def dateParse(str: String): Instant = Instant.parse(str ++ "T00:00:00.00Z")

}
