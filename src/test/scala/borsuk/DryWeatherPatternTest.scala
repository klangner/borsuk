package borsuk

import java.time.LocalDate

import org.scalatest.{Matchers, WordSpec}
import carldata.borsuk.rdiis.DryWeatherPattern

class DryWeatherPatternTest extends WordSpec with Matchers {

  "The dry weather pattern" should {

    "find dry weather day before rain started" in {

      val day = LocalDate.of(2018, 1, 1) //Monday
      val allDwpDays = Seq(LocalDate.of(2017, 12, 31), //Sunday
        LocalDate.of(2017, 12, 30), //Saturday
        LocalDate.of(2017, 12, 29), //Friday
        LocalDate.of(2017, 12, 28), //Thursday
        LocalDate.of(2017, 12, 27), //Wednesday
        LocalDate.of(2017, 12, 26), //Tuesday
        LocalDate.of(2017, 12, 25), //Monday
      )
      val dayWithDryDayPattern = DryWeatherPattern.findDryDay2(day, allDwpDays)
      dayWithDryDayPattern.get shouldBe LocalDate.of(2017, 12, 25)
    }

    "find dry weather day after rain started" in {

      val day = LocalDate.of(2018, 1, 1) // Monday
      val allDwpDays = Seq(LocalDate.of(2018, 1, 8), //Monday
        LocalDate.of(2018, 1, 9), //Tuesday
        LocalDate.of(2018, 1, 10), //Wednesday
        LocalDate.of(2018, 1, 11), //Thursday
        LocalDate.of(2018, 1, 12), //Friday
        LocalDate.of(2018, 1, 13), //Saturday
        LocalDate.of(2018, 1, 14), //Sunday
      )

      val dayWithDryDayPattern = DryWeatherPattern.findDryDay2(day,allDwpDays)
      dayWithDryDayPattern.get shouldBe LocalDate.of(2018,1,8)
    }

  }

}
