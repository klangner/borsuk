package carldata.borsuk.prediction

import java.time._

import carldata.series.{Patterns, TimeSeries}

case class trainedModel(features: TimeSeries[(Double, Double)])

object DailyPatternModel {
  def fit(flow: TimeSeries[Double]): trainedModel = {
    val duration = Duration.ofMinutes(5)

    def f(x1: (Instant, Double), x2: (Instant, Double), tsh: Instant): Double = {
      val tx = Duration.between(tsh, x1._1).toMillis
      val ty = Duration.between(tsh, x2._1).toMillis
      (ty / (tx + ty) * x1._2) + (tx / (tx + ty) * x2._2)
    }

    val xs = TimeSeries.interpolate(flow, duration).addMissing(duration, f)
    trainedModel(Patterns.daily(xs))
  }


  def predict(day: LocalDate, dailyPattern: trainedModel): trainedModel = {
    val idx = dailyPattern.features.idx.map { x =>
      LocalDateTime.of(day, LocalTime.MIDNIGHT)
        .plusSeconds(x.getEpochSecond)
        .toInstant(ZoneOffset.UTC)
    }
    trainedModel(new TimeSeries[(Double, Double)](idx, dailyPattern.features.values))
  }
}
