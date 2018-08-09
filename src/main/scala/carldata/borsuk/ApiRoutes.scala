package carldata.borsuk

import java.time.Instant
import java.util.UUID.randomUUID

import akka.http.scaladsl.model.{HttpMethods, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import carldata.series.{Csv, TimeSeries}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings
import spray.json._

import scala.collection.immutable.Seq
import scala.language.postfixOps

case class Parameters(project: String, flow: String, projectsUrl: String)

class ApiRoutes(storage: Storage) {
  val models = collection.mutable.Map.empty[String, String]
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
            if (body.isEmpty) complete("type:error\nEmpty body.")
            else {
              val json = body.parseJson.asJsObject
              if (json.fields.contains("type")) {
                val id = randomUUID().toString
                models += id -> "props"

                complete("{\"id\": \"" + id + "\"}")
              }
              else complete("type:error\nModel type not provided.")
            }
        }
      }
    } ~ path("api" / "model" / Segment / "fit") {
      id =>
        post {
          entity(as[String]) {
            body =>
              if (models.contains(id)) {
                if (body.isEmpty) complete("type:error\nEmpty body.")
                else {
                  val json = body.parseJson.asJsObject
                  if (json.fields.contains("features") && json.fields.contains("labels")) {
                    complete(StatusCodes.OK)
                  }
                  else complete("type:error\nFeatures or labels not provided.")
                }
              }
              else complete(StatusCodes.NotFound)
          }
        }
    } ~ path("api" / "model" / Segment / "predict") {
      id =>
        post {
          entity(as[String]) {
            body =>
              if (models.contains(id)) {
                if (body.isEmpty) complete("type:error\nEmpty body.")
                else {
                  val json = body.parseJson.asJsObject
                  if (json.fields.contains("features")) {
                    complete("{\n  \"labels\": [1,2,3]\n}")
                  }
                  else complete("type:error\nFeatures not provided.")

                }
              }
              else complete(StatusCodes.NotFound)
          }
        }
    } ~ path("api" / "model" / Segment / "status") {
      id =>
        post {
          if (models.contains(id)) {
            complete("{\n  \"build\": \"1\"\n}")
          }
          else complete(StatusCodes.NotFound)
        }
    }
  }


  def anomaly(ts: TimeSeries[Double]): StandardRoute = {
    val cleanedTs = Anomaly.fixAnomalies(ts)
    complete(Csv.toCsv(cleanedTs))
  }

  def dateParse(str: String): Instant = Instant.parse(str ++ "T00:00:00.00Z")

}
