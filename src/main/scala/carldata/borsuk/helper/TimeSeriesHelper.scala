package carldata.borsuk.helper

import java.time.Instant

import carldata.series.TimeSeries

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
}
