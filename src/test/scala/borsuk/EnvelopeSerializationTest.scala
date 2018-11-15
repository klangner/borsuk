package borsuk

import java.time.{Duration, LocalDateTime}

import org.scalatest.{Matchers, WordSpec}
import spray.json._
import carldata.borsuk.envelope.EnvelopeResult
import carldata.borsuk.envelope.EnvelopeResultJsonProtocol._
import carldata.series.Sessions
import carldata.borsuk.helper.DateTimeHelper.dtToInstant


class EnvelopeSerializationTest extends WordSpec with Matchers {

  "EnvelopeResult" should {

    "serialize and deserialize back to proper EnvelopeResult" in {

      val sessions = Seq(
        Sessions.Session(
          dtToInstant(LocalDateTime.of(2018, 1, 1, 8, 5, 15)),
          dtToInstant(LocalDateTime.of(2018, 1, 1, 10, 7, 19))),
        Sessions.Session(
          dtToInstant(LocalDateTime.of(2018, 1, 2, 9, 3, 37)),
          dtToInstant(LocalDateTime.of(2018, 1, 2, 12, 9, 13))),
      )

      val dataPoints = sessions.zip(Seq(1.0, 2.0)).zip(Seq(3.0, 4.0))
      val envelopeResult = new EnvelopeResult(dataPoints, Seq(1.0, 2.0, 3.0), Duration.parse("PT12H"))

      println(envelopeResult.toJson.prettyPrint)

      val envelopeResult2 = envelopeResult.toJson.toString.parseJson.convertTo[EnvelopeResult]
      envelopeResult.slope shouldEqual envelopeResult2.slope
      envelopeResult.intercept shouldEqual envelopeResult2.intercept
      envelopeResult.rSquare shouldEqual envelopeResult2.rSquare
      envelopeResult.dataPoints should equal (envelopeResult2.dataPoints)
      envelopeResult.rainfall should equal (envelopeResult2.rainfall)
      envelopeResult.flows should equal (envelopeResult2.flows)
      envelopeResult.sessionWindow shouldEqual envelopeResult2.sessionWindow
      envelopeResult.dates should equal (envelopeResult2.dates)
      envelopeResult.dates(0).startIndex shouldEqual envelopeResult2.dates(0).startIndex
      envelopeResult.dates(0).endIndex shouldEqual envelopeResult2.dates(0).endIndex
    }

    "should be the same after serialization and deserialization of HashMap" in {
      0 shouldEqual 1
    }

  }

}
