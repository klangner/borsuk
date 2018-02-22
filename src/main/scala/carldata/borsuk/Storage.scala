package carldata.borsuk

import java.time.LocalDate

import carldata.series.TimeSeries

case class Project(name: String, flow: Seq[String], rain: Seq[String],
                   startDate: LocalDate, endDate: LocalDate, splitDate: LocalDate)

trait Storage {
  def getProject(project: String): Project

  def getTimeSeries(project: String, flow: String): TimeSeries[Double]
}