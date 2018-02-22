package carldata.borsuk

import java.time.{Instant, LocalDate}

import akka.http.scaladsl.model.HttpMethods
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Route, StandardRoute}
import carldata.series.{Csv, TimeSeries}
import ch.megard.akka.http.cors.scaladsl.CorsDirectives.cors
import ch.megard.akka.http.cors.scaladsl.settings.CorsSettings

import scala.collection.immutable.Seq


case class Parameters(project: String, flow: String, projectsUrl: String)


object ApiRoutes {

  val settings: CorsSettings.Default = CorsSettings.defaultSettings.copy(allowedMethods = Seq(
    HttpMethods.GET,
    HttpMethods.POST,
    HttpMethods.DELETE,
    HttpMethods.HEAD,
    HttpMethods.OPTIONS))

  /** Routing */
  def route(storage: Storage): Route = cors(settings) {
    path("api" / "healthcheck") {
      complete("Ok")
    } ~ (path("api" / "prediction" / Remaining) & parameters("flow".as[String], "rain" ?, "day".as[String])) {
      (project, flow, rain, day) =>
        get {
          val flowTs = storage.getTimeSeries(project, flow)
          val rainTs = if (rain.isDefined) storage.getTimeSeries(project, rain.get) else TimeSeries.empty[Double]
          new ApiRoutes(storage, flowTs, rainTs).predict(day)
        }
    }
  }
}

class ApiRoutes(storage: Storage, flow: TimeSeries[Double], rain: TimeSeries[Double]) {
  def predict(day: String): StandardRoute = {
    val partialFlow = takeToDate(flow, day)
    val partialRain = takeToDate(rain, day)
    val prediction = Prediction.fit(partialFlow, partialRain).predict(LocalDate.parse(day))
    complete(Csv.toCsv(prediction))
  }

  def dateParse(str: String): Instant = Instant.parse(str ++ "T00:00:00.00Z")

  def takeToDate(ts: TimeSeries[Double], day: String): TimeSeries[Double] = {
    if (ts.isEmpty) ts else ts.slice(ts.index.head, dateParse(day))
  }
}
