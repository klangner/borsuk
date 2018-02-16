package carldata.borsuk

import java.time._

import carldata.series.{Gen, TimeSeries}

object Prediction {
  def fit(ts: TimeSeries[Double]): Prediction = {
    new Prediction(ts)
  }
}

class Prediction(ts: TimeSeries[Double]) {
  def predict(day: LocalDate): TimeSeries[Double] = {
    val idx = (for (i <- 0 to 287) yield
      LocalDateTime.of(day, LocalTime.MIDNIGHT)
        .plusSeconds(60 * 60 * i)
        .toInstant(ZoneOffset.UTC)).toVector

    Gen.randomNoise(idx, 0.5, 0.7)
  }

}
