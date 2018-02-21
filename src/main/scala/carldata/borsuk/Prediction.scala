package carldata.borsuk

import java.time._

import carldata.series.{Patterns, TimeSeries}


object Prediction {
  def fit(ts: TimeSeries[Double]): Prediction = {
    val duration = Duration.ofMinutes(5)

    def f(x1: (Instant, Double), x2: (Instant, Double), tsh: Instant): Double = {
      val tx = Duration.between(tsh, x1._1).toMillis
      val ty = Duration.between(tsh, x2._1).toMillis
      (ty / (tx + ty) * x1._2) + (tx / (tx + ty) * x2._2)
    }

    val xs = TimeSeries.interpolate(ts, duration).addMissing(duration, f)
    val dailyPattern = Patterns.daily(xs)
    new Prediction(dailyPattern)
  }
}

class Prediction(dailyPattern: TimeSeries[Double]) {
  def predict(day: LocalDate): TimeSeries[Double] = {
    val idx = dailyPattern.idx.map { x =>
      LocalDateTime.of(day, LocalTime.MIDNIGHT)
        .plusSeconds(x.getEpochSecond)
        .toInstant(ZoneOffset.UTC)
    }
    new TimeSeries[Double](idx, dailyPattern.values)
  }

}
