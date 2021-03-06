package carldata.borsuk.rdiis

import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.helper.DateTimeHelper._
import carldata.series.TimeSeries

object DryWeatherPattern {

  def findAllDryDays(rainfall: TimeSeries[Double], dryDayWindow: Duration): Seq[LocalDate] = {
    rainfall
      .groupByTime(_.truncatedTo(ChronoUnit.DAYS), _.unzip._2.sum)
      .rollingWindow(dryDayWindow, _.sum)
      .filter(x => x._2 < 1.0)
      .index
      .map(instantToDay)
  }

  /**
    * Select date of DWP
    */
  def findDryDay(day: LocalDate, dryDays: Seq[LocalDate]): Option[LocalDate] = {
    val dryDay = dryDays.filter(x => x.isBefore(day))
      .filter(x => isDateHoliday(x) == isDateHoliday(day))
      .lastOption

    if(dryDay.isEmpty){
      dryDays.filter(x => x.isAfter(day))
        .find(x => isDateHoliday(x) == isDateHoliday(day))
    } else {
      dryDay
    }
  }

  def get(dwpDay: LocalDate, flow: TimeSeries[Double]): TimeSeries[Double] = {
    flow.filter(x => instantToDay(x._1) == dwpDay)
  }
}
