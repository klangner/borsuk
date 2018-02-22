package carldata.borsuk

import java.time.LocalDate

import carldata.series.TimeSeries

case class Project(name: String, flow: Seq[String], rain: Seq[String],
                   startDate: LocalDate, endDate: LocalDate, splitDate: LocalDate)

trait Storage {
  def getProject(projectName: String): Project

  def getTimeSeries(projectName: String, seriesName: String): TimeSeries[Double]
}