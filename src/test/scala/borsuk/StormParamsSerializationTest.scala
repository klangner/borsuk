package borsuk

import java.time.{LocalDateTime, Duration}

import org.scalatest.{Matchers, WordSpec}
import scala.collection.immutable
import carldata.borsuk.storms.Storms.StormParams
import carldata.borsuk.storms.StormParamsJsonProtocol._
import carldata.borsuk.storms.StormParamsHashMapJsonProtocol._
import carldata.series.Sessions.Session
import carldata.borsuk.helper.DateTimeHelper._
import spray.json._


class StormParamsSerializationTest extends WordSpec with Matchers {

  val stormParams = StormParams(
    Session(dtToInstant(LocalDateTime.of(2018, 1, 1, 0, 0, 0)),
      dtToInstant(LocalDateTime.of(2018, 1, 2, 0, 0, 0))),
    Seq(Duration.parse("PT12H")),
    Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0)
  )

  "StormParams" should {
    "serialize to json and deserialize to proper Storm Params object" in {
      val stormParamsJson = stormParams.toJson
      val stormParamsDeserialized = stormParamsJson.convertTo[StormParams]

      stormParamsDeserialized.session.startIndex shouldBe stormParams.session.startIndex
      stormParamsDeserialized.session.endIndex shouldBe stormParams.session.endIndex
      stormParamsDeserialized.sessionWindow shouldBe stormParams.sessionWindow
      stormParamsDeserialized.values.length shouldBe stormParams.values.length
      stormParamsDeserialized.values(0) shouldBe stormParams.values(0)
      stormParamsDeserialized.values(4) shouldBe stormParams.values(4)
      stormParamsDeserialized.values(9) shouldBe stormParams.values(9)
    }
  }

  "Storm Params Hash map" should {
    "serialize to json and deserialize to proper Storm Params Hash Map" in {
      val stormParamsHashMap = immutable.HashMap(("key1", stormParams), ("key2", stormParams))
      val stormParamsJsonHashMap = stormParamsHashMap.toJson.toString

      val stormParamsHashMapDeserialized = stormParamsJsonHashMap.parseJson.convertTo[immutable.HashMap[String, StormParams]]

      stormParamsHashMapDeserialized("key2").session.startIndex shouldBe stormParams.session.startIndex
      stormParamsHashMapDeserialized("key2").session.endIndex shouldBe stormParams.session.endIndex
      stormParamsHashMapDeserialized("key2").sessionWindow shouldBe stormParams.sessionWindow
      stormParamsHashMapDeserialized("key2").values.length shouldBe stormParams.values.length
      stormParamsHashMapDeserialized("key2").values(0) shouldBe stormParams.values(0)
      stormParamsHashMapDeserialized("key2").values(4) shouldBe stormParams.values(4)
      stormParamsHashMapDeserialized("key2").values(9) shouldBe stormParams.values(9)
    }
  }
}
