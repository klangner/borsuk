package borsuk

import java.time.Instant

import carldata.borsuk.Anomaly
import carldata.series.TimeSeries
import org.scalatest.{FlatSpec, Matchers}

class AnomalyTest extends FlatSpec with Matchers {

  "Anomaly" should "return cleaned series" in {
    val now = Instant.now
    val idx = Vector(1, 2, 3, 4, 5).map(i => now.plusSeconds(i))
    val vs: Vector[Double] = Vector(1, 0, 3, -9, 5)
    val vs2: Vector[Double] = Vector(1, 2, 3, 4, 5)
    val ts: TimeSeries[Double] = new TimeSeries(idx, vs)
    Anomaly.fixAnomalies(ts) shouldBe new TimeSeries(idx, vs2)
  }

  it should "return series with right length" in {
    val now = Instant.now
    val idx = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9).map(i => now.plusSeconds(i))
    val vs: Vector[Double] = Vector(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val ts: TimeSeries[Double] = new TimeSeries(idx, vs)
    Anomaly.fixAnomalies(ts).length shouldBe ts.length
  }

}
