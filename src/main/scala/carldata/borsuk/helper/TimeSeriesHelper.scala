package carldata.borsuk.helper

import java.time.{Duration, Instant, LocalDateTime}

import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import carldata.borsuk.helper.DateTimeHelper.{dtToInstant, instantToLDT}
import carldata.series.{Gen, TimeSeries}

object TimeSeriesHelper {

  def adjust(raw: TimeSeries[Double], template: TimeSeries[Double]): TimeSeries[Double] = {
    val rs = if (raw.nonEmpty && template.nonEmpty) {
      val leftCorrected = if (raw.head.get._1.isAfter(template.head.get._1)) (template.head.get._1, 0.0) else raw.head.get
      val rightCorrected = if (raw.last.get._1.isBefore(template.last.get._1)) (template.last.get._1, 0.0) else raw.last.get
      (leftCorrected +: raw.dataPoints :+ rightCorrected).distinct.unzip
    }
    else if (template.nonEmpty) {
      (Vector(template.head.get._1, template.last.get._1), Vector(0.0, 0.0))
    }
    else (Vector(), Vector())

    TimeSeries(rs._1, rs._2)
  }

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

  def toTimeSeriesParams(ts: TimeSeries[Double]): TimeSeriesParams = {
    if (ts.isEmpty) TimeSeriesParams(LocalDateTime.now(), Duration.ZERO, Array())
    else TimeSeriesParams(instantToLDT(ts.index.head), ts.resolution, ts.values.toArray)
  }
}
