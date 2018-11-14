package carldata.borsuk.helper

import java.time.{Instant, LocalDateTime}

import carldata.series.{Gen, TimeSeries}
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.helper.DateTimeHelper.dtToInstant

object TimeSeriesHelper {
  def concat[V](xs: Seq[TimeSeries[V]]): TimeSeries[V] = {
    if (xs.isEmpty) TimeSeries.empty[V]
    else xs.foldLeft(TimeSeries.empty[V]) { (ts, ts2) =>
      new TimeSeries[V](ts.index ++ ts2.index, ts.values ++ ts2.values)
    }
      .sortByIndex
  }

  def first[V](xs: TimeSeries[V], f: ((Instant, V)) => Boolean): Option[(Instant, V)] = {
    if (xs.isEmpty) None
    else {
      val xs2 = xs.filter(f)

      if (xs2.isEmpty) None
      else xs2.head
    }
  }

  def parse(tsp: TimeSeriesParams): TimeSeries[Double] = {
    val ed: LocalDateTime = tsp.startDate.plusSeconds(tsp.resolution.getSeconds * tsp.values.length)
    val idx: Seq[Instant] = Gen.mkIndex(dtToInstant(tsp.startDate), dtToInstant(ed), tsp.resolution)
    TimeSeries(idx.toVector, tsp.values.toVector)
  }
}
