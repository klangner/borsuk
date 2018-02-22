package carldata.borsuk.Storages

import java.time.LocalDateTime

import carldata.borsuk.{JsonConverters, Project, Storage}
import carldata.series.{Csv, TimeSeries}
import spray.json._

import scala.io.Source

class GoogleCloudStorage(projectsUrl: String) extends Storage {

  override def getProject(project: String): Project = {

    val url: String = "https://" ++ projectsUrl ++ "/" ++ project.replace("/", "") ++ "/" ++ "project.json"
    val json = Source.fromURL(url).mkString.parseJson.asJsObject.fields

    val startDate = json.get("start-date").map(JsonConverters.timestampFromValue).getOrElse(LocalDateTime.MIN).toLocalDate
    val endDate = json.get("end-date").map(JsonConverters.timestampFromValue).getOrElse(LocalDateTime.MIN).toLocalDate
    val splitDate = json.get("split-date").map(JsonConverters.timestampFromValue).getOrElse(LocalDateTime.MIN).toLocalDate
    val flows = JsonConverters.arrayFromValue(json.getOrElse("flows", JsArray()))
    val rainfalls = JsonConverters.arrayFromValue(json.getOrElse("rainfalls", JsArray()))


    Project(project, flows, rainfalls, startDate, endDate, splitDate)
  }

  override def getTimeSeries(project: String, xs: String): TimeSeries[Double] = {
    val url: String = "https://" ++ projectsUrl ++ "/" ++ project.replace("/", "") ++ "/" ++ xs ++ ".csv"
    val csv = Source.fromURL(url)
    Csv.fromString(csv.mkString)
  }
}
