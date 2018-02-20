package carldata.borsuk

import java.time._
import java.time.temporal.{ChronoUnit, TemporalUnit}

import carldata.series.{Patterns, TimeSeries}


object Prediction {
  def fit(ts: TimeSeries[Double]): Prediction = {
    new Prediction(ts)
  }
}

class Prediction(ts: TimeSeries[Double]) {
  def predict(day: LocalDate): TimeSeries[Double] = {
    val instDay = LocalDateTime.of(day, LocalTime.MIDNIGHT).toInstant(ZoneOffset.UTC)
    val duration = Duration.ofMinutes(5)

    def f(x1: (Instant, Double), x2: (Instant, Double), tsh: Instant): Double = {
      val tx = Duration.between(tsh, x1._1).toMillis
      val ty = Duration.between(tsh, x2._1).toMillis
      (ty / (tx + ty) * x1._2) + (tx / (tx + ty) * x2._2)
    }

    def g(vs: TimeSeries[Double]): TimeSeries[Double] = {
      if (vs.idx.last.isAfter(instDay)) {
        vs.filter(x => LocalDateTime.ofInstant(x._1, ZoneOffset.UTC).toLocalDate.equals(day))
      }
      else {
        val xs = Patterns.daily(vs)
        val idx = xs.idx.map { x =>
          LocalDateTime.ofInstant(vs.take(vs.length - 3).idx.last, ZoneOffset.UTC)
            .truncatedTo(ChronoUnit.DAYS)
            .plusSeconds(x.getEpochSecond + 86400)
            .toInstant(ZoneOffset.UTC)
        }
        g(new TimeSeries(vs.idx ++ idx, vs.values ++ xs.values))
      }
    }

    val xs = TimeSeries.interpolate(ts, duration).addMissing(duration, f)
    g(xs)
  }

}
