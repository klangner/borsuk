package carldata.borsuk.helper

import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant}

import carldata.series.{Outliers, Stats, TimeSeries}

trait Type

case object Rain extends Type

case object Flow extends Type



object Cleaner {
  /**
    *
    * @param ts    series to clean
    * @param tp    Rain or Flow?
    * @param delta duration between two indexes (for long series this could be ts.resolution)
    * @return
    */
  def run(ts: TimeSeries[Double], tp: Type, delta: Duration = Duration.ofMinutes(5)): TimeSeries[Double] = {
    val tsWithoutOutliers: TimeSeries[Double] = if (tp == Rain) {
      //rain
      ts.filter(x => x._2 >= 0)
    }
    else {
      //flow
      val stddev = Math.sqrt(Stats.meanAndVariance(ts.values).variance)
      Outliers.remove(ts, 0, 3 * stddev)
    }
    resample(tsWithoutOutliers, delta)
  }

  def resample(ts: TimeSeries[Double], delta: Duration): TimeSeries[Double] = {

    /**
      * Find the rounded date. Rounded by provided delta,
      * ie.: if ct have date with 17 minutes and we round it to 5 minutes duration,
      * result will equal 15 in minutes part of date.
      */
    def floor(ct: Instant): Instant = {
      val st = ts.index.head
      val diff = ChronoUnit.SECONDS.between(st, ct)
      st.plusSeconds((diff / delta.getSeconds) * delta.getSeconds)
    }

    // Error is -1, and should be handled in borsuk and other tools which use this module
    val tsWithErrors: TimeSeries[Double] = ts.addMissing(delta, (_, _, _) => -1.0)
      .groupByTime(floor, _.unzip._2.find(_ != -1.0).getOrElse(-1.0))

    val interpolatedTs: TimeSeries[Double] = TimeSeries.interpolate(ts, delta)

    tsWithErrors.join(interpolatedTs)
      .mapValues { x =>
        if (x._1 != x._2 && x._1 != -1) x._2 else x._1
      }
  }

}
