package borsuk

import java.time.{LocalDateTime, Duration}
import carldata.borsuk.BasicApiObjects.TimeSeriesParams
import org.scalatest.{FlatSpec, Matchers}
import carldata.borsuk.storms.ApiObjects._
import carldata.borsuk.storms.Storms

class StormsTest extends FlatSpec with Matchers {

  "Storms" should "have model buildNumber equal 0 for fit with empty time series" in {
    val storms = new Storms("testModelType", "testId")
    storms.fit(FitStormsParams(TimeSeriesParams(LocalDateTime.now(), Duration.ofMillis(1000), Array())))
    storms.buildNumber shouldBe 0
  }

}
