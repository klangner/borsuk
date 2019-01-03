package borsuk

import java.time.{Duration, Instant, LocalDateTime}

import carldata.borsuk.helper.DateTimeHelper._
import carldata.borsuk.rdiis.RDIIObject
import carldata.borsuk.rdiis.RDIIObjectHashMapJsonProtocol._
import carldata.borsuk.rdiis.RDIIObjectJsonProtocol._
import carldata.series.Sessions.Session
import carldata.series.TimeSeries
import org.scalatest.{Matchers, WordSpec}
import spray.json._

import scala.collection.immutable

class RDIISerializationTest extends WordSpec with Matchers {

  "RDIIObject" should {

    val startDate = LocalDateTime.now
    val ts = TimeSeries(
      (0 until 3).map(i => dtToInstant(startDate.plusMinutes(i * 5))).toVector,
      Vector[Double](1.0, 2.0, 3.0))

    val rdiiObject = RDIIObject(Duration.parse("P2D"),
      ts,
      ts,
      ts,
      ts,
      Seq("1", "2", "3"),
      Session(Instant.EPOCH, Instant.EPOCH))

    "serialize to json and deserialize back to proper RDIIObject" in {

      val rdiiObjectDeserialized = rdiiObject.toJson.convertTo[RDIIObject]

      rdiiObjectDeserialized.sessionWindow shouldBe Duration.parse("P2D")

      rdiiObjectDeserialized.rainfall.length shouldBe 3
      instantToLDT(rdiiObjectDeserialized.rainfall.index(0)) shouldBe startDate
      instantToLDT(rdiiObjectDeserialized.rainfall.index(1)) shouldBe startDate.plusMinutes(5)
      instantToLDT(rdiiObjectDeserialized.rainfall.index(2)) shouldBe startDate.plusMinutes(10)
      rdiiObjectDeserialized.rainfall.values(0) shouldBe 1.0
      rdiiObjectDeserialized.rainfall.values(1) shouldBe 2.0
      rdiiObjectDeserialized.rainfall.values(2) shouldBe 3.0

      rdiiObjectDeserialized.childIds.length shouldBe 3
      rdiiObjectDeserialized.childIds(0) shouldBe "1"
      rdiiObjectDeserialized.childIds(2) shouldBe "3"


    }

    "should be the same before and after serialization of hash map" in {

      val z = immutable.HashMap(("key1", rdiiObject), ("key2", rdiiObject))
      val hashMap = z.toJson.convertTo[immutable.HashMap[String, RDIIObject]]

      val rdiiObjectFromMap = hashMap("key1")
      rdiiObjectFromMap.rainfall.length shouldBe 3
      instantToLDT(rdiiObjectFromMap.rainfall.index(0)) shouldBe startDate
      instantToLDT(rdiiObjectFromMap.rainfall.index(1)) shouldBe startDate.plusMinutes(5)
      instantToLDT(rdiiObjectFromMap.rainfall.index(2)) shouldBe startDate.plusMinutes(10)
      rdiiObjectFromMap.rainfall.values(0) shouldBe 1.0
      rdiiObjectFromMap.rainfall.values(1) shouldBe 2.0
      rdiiObjectFromMap.rainfall.values(2) shouldBe 3.0
    }
  }

}
