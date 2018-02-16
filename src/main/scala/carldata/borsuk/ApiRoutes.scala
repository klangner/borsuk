package carldata.borsuk

import java.time.Instant

import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.StandardRoute
import carldata.series.{Csv, TimeSeries}

import scala.io.Source

object ApiRoutes {
  def predict(project: String, flow: String, day: String, projectsUrl: String): StandardRoute = {
    val url: String = "https://" ++ projectsUrl ++ "/" ++ project ++ "/" ++ flow ++ ".csv"
    val csv = Source.fromURL(url)
    val ts: TimeSeries[Double] = Csv.fromString(csv.mkString)
    val ts2: TimeSeries[Double] = ts.slice(ts.index.head, dateParse(day))
    complete(Csv.toCsv(Prediction.predict))
  }

  def dateParse(str: String): Instant = Instant.parse(str ++ "T00:00:00.00Z")

}
