package carldata.borsuk.Storages

import carldata.borsuk.{JsonConverters, Project, Storage}
import carldata.series.{Csv, TimeSeries}
import spray.json._

import scala.io.Source

class GoogleCloudStorage(projectsUrl: String) extends Storage {

  override def getProject(project: String): Project = {

    val url: String = "https://" ++ projectsUrl ++ "/" ++ project.replace("/", "") ++ "/" ++ "project.json"
    val json = Source.fromURL(url).mkString.parseJson.asJsObject.fields

    val startDate = JsonConverters.timestampFromValue(json.getOrElse("start-date", JsString(""))).toLocalDate
    val endDate = JsonConverters.timestampFromValue(json.getOrElse("end-date", JsString(""))).toLocalDate
    val splitDate = JsonConverters.timestampFromValue(json.getOrElse("split-date", JsString(""))).toLocalDate
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
