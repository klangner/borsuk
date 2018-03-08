package carldata.borsuk

import java.time.{Instant, LocalDate}

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
    } ~ (path("api" / "prediction" / Remaining) & parameters("flow".as[String], "rain" ?, "day".as[String])) {
      (project, flow, rain, day) =>
        get {
          val flowTs = storage.getTimeSeries(project, flow)
          val rainTs = if (rain.isDefined) storage.getTimeSeries(project, rain.get) else TimeSeries.empty[Double]
          predict(day, flowTs, rainTs)
        }
    } ~ (path("api" / "anomaly" / Remaining) & parameters("series".as[String])) {
      (project, series) =>
        get {
          val ts = storage.getTimeSeries(project, series)
          anomaly(ts)
        }
    }
  }

  def predict(day: String, flow: TimeSeries[Double], rain: TimeSeries[Double]): StandardRoute = {
    val partialFlow = flow.slice(flow.index.head, dateParse(day))
    val prediction = Prediction.fit(partialFlow, rain).predict(LocalDate.parse(day))
    complete(Csv.toCsv(prediction))
  }

  def anomaly(ts: TimeSeries[Double]): StandardRoute = {
    val cleanedTs = new Anomaly(ts).find
    complete(Csv.toCsv(cleanedTs))
  }

  def dateParse(str: String): Instant = Instant.parse(str ++ "T00:00:00.00Z")

}
