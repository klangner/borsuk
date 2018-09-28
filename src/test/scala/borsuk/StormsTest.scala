package borsuk

import java.time.{Duration, LocalDateTime, Month}

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

  val storms = new Storms("testModelType", "testId")
  val x = TimeSeriesParams(LocalDateTime.of(1982, Month.JULY, 24, 0, 0, 0),
    Duration.ofMinutes(5),
    Array(1.0, 2.0, 3.0, 0.0, 0.0, 4.0, 5.0, 0.0, 6.0))
  storms.fit(FitStormsParams(x))

  it should "have model with 3 basic storms" in {

    storms.list().map(x => storms.get(x._1))
      .foreach(x => {
        println(("--------------"))
        println(x.get._3.mkString("\n"))
      })

    storms.list().length shouldBe 3

  }

  "First model" should "have values 1.0, 2.0, 3.0" in {
    storms.get("0").get._3(0) shouldBe 1.0
    storms.get("0").get._3(1) shouldBe 2.0
    storms.get("0").get._3(2) shouldBe 3.0
  }

  "Second model" should "have values 4.0, 5.0" in {
    storms.get("1").get._3(0) shouldBe 4.0
    storms.get("1").get._3(1) shouldBe 5.0
  }

  "Third model" should "have value 6.0" in {
    storms.get("2").get._3(0) shouldBe 6.0
  }
}
