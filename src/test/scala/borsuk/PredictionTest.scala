package borsuk

import java.time.{Duration, Instant, LocalDate}

import carldata.borsuk.Prediction
import carldata.series.{Gen, TimeSeries}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random


class PredictionTest extends FlatSpec with Matchers {
  "Prediction" should "return series with right length" in {
    val idx = Gen.mkIndex(Instant.EPOCH, Instant.ofEpochSecond(60 * 60 * 24), Duration.ofSeconds(1))
    val ts: TimeSeries[Double] = Gen.randomNoise(idx, 5, 9, new Random(100))
    Prediction.fit(ts, TimeSeries.empty).predict(LocalDate.of(1970, 2, 2)).length shouldBe 288
  }

  it should "return series with for right date" in {
    val idx = Gen.mkIndex(Instant.EPOCH, Instant.ofEpochSecond(60 * 60 * 24), Duration.ofSeconds(1))
    val ts: TimeSeries[Double] = Gen.randomNoise(idx, 5, 9, new Random(100))
    Prediction.fit(ts, TimeSeries.empty).predict(LocalDate.of(1970, 2, 2)).index.head shouldBe Instant.parse("1970-02-02T00:00:00.00Z")
  }
}
