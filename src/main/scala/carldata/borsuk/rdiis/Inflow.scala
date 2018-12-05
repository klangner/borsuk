package carldata.borsuk.rdiis

import java.time._
import java.time.temporal.ChronoUnit

import carldata.borsuk.rdiis.DryWeatherPattern._
import carldata.borsuk.helper.DateTimeHelper._
import carldata.series.Sessions.Session
import carldata.series.TimeSeries

object Inflow {

  /**
    * Find inflows in session
    */
  def fromSession(session: Session, flow: TimeSeries[Double], dryDays: Seq[LocalDate]): TimeSeries[Double] = {
    def moveBorder(sessionBorder: Instant): Instant = dayToInstant(instantToDay(sessionBorder))

    val slicedFlow = flow.slice(moveBorder(session.startIndex).minus(2, ChronoUnit.DAYS), moveBorder(session.endIndex.plus(1, ChronoUnit.DAYS)))
    val sessionDays = slicedFlow.groupByTime(_.truncatedTo(ChronoUnit.DAYS), _ => identity(0.0)).index.map(instantToDay)
    val patternDays = sessionDays.map(x => (x, findDryDay(x, dryDays)))

    val patternInflows: Seq[(LocalDate, TimeSeries[Double])] = patternDays.map(x => (x._1, DryWeatherPattern.get(x._2.getOrElse(LocalDate.MAX), flow)))
    val res = slicedFlow.map { x =>
      val (d, t) = (instantToDay(x._1), instantToTime(x._1))
      val patternValue = patternInflows.filter(p => p._1 == d).head._2
        .filter(p => instantToTime(p._1) == t)
        .head.map(_._2).getOrElse(Double.NaN)
      if (patternValue.isNaN) Double.NaN
      else x._2 - patternValue
    }
      .filter(x => !x._2.isNaN)
      .mapValues(Math.max(_, 0.0))
      res
  }

  /**
    * Give maximum intensity in inflow series
    */
  def intensity(inflows: TimeSeries[Double]): Double = {
    if (inflows.isEmpty) 0.0
    else inflows.values.max
  }

}
