package borsuk

import java.time.{Duration, Instant, LocalDate, LocalDateTime}

import carldata.borsuk.helper.DateTimeHelper.dtToInstant
import carldata.borsuk.rdiis.Inflow
import carldata.series.Sessions.Session
import carldata.series.{Gen, TimeSeries}
import org.scalatest.{Matchers, WordSpec}

//import carldata.borsuk.rdiis

class InflowTest extends WordSpec with Matchers {

  "The Inflow" should {
    "subtract to zero" in {
      val sd = LocalDateTime.of(2018, 10, 1, 0, 0, 0)
      val resolution = Duration.ofDays(1)
      val values = Array(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0)
      val allDWPDays = Seq(LocalDate.of(2018, 10, 1)
        , LocalDate.of(2018, 10, 2)
        , LocalDate.of(2018, 10, 3)
        , LocalDate.of(2018, 10, 4)
        , LocalDate.of(2018, 10, 5)
        , LocalDate.of(2018, 10, 6)
        , LocalDate.of(2018, 10, 7)
      )
      val ed: LocalDateTime = sd.plusSeconds(resolution.getSeconds * values.length)
      val index: Seq[Instant] = Gen.mkIndex(dtToInstant(sd), dtToInstant(ed), resolution)
      val flow: TimeSeries[Double] = TimeSeries(index.toVector, values.toVector)


      val inflow = Inflow.fromSession(Session(dtToInstant(sd.plusDays(7)), dtToInstant(ed)), flow, allDWPDays)

      inflow.length shouldEqual 9
      inflow.values.head shouldEqual 0.0
      inflow.values.last shouldEqual 0.0
    }
  }
}
