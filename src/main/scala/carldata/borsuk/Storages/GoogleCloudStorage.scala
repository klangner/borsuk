package carldata.borsuk.Storages

import java.time.LocalDate

import carldata.borsuk.{Project, Storage}
import carldata.series.{Csv, TimeSeries}

import scala.io.Source
import spray.json._

class GoogleCloudStorage(projectsUrl: String) extends Storage {
  override def getProject(project: String): Project = {
    def getDate(m: Map[String, JsValue], date: String) = {
      LocalDate.parse(m.getOrElse(date, "").toString.replace("\"", ""))
    }

    def getSeq(m: Map[String, JsValue], arr: String) = {
      m.getOrElse("flows", JsArray()).asInstanceOf[JsArray].elements.map(_.toString)
    }

    val url: String = "https://" ++ projectsUrl ++ "/" ++ project.replace("/", "") ++ "/" ++ "project.json"
    val json = Source.fromURL(url).mkString.parseJson.asJsObject.fields


    val startDate = getDate(json, "start-date")
    val endDate = getDate(json, "end-date")
    val splitDate = getDate(json, "split-date")
    val flows = getSeq(json, "flows")
    val rainfalls = getSeq(json, "rainfalls")
    Project(project, flows, rainfalls, startDate, endDate, splitDate)
  }

  override def getTimeSeries(project: String, xs: String): TimeSeries[Double] = {
    val url: String = "https://" ++ projectsUrl ++ "/" ++ project.replace("/", "") ++ "/" ++ xs ++ ".csv"
    val csv = Source.fromURL(url)
    Csv.fromString(csv.mkString)
  }
}
