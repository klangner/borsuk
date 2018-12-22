package carldata.borsuk.envelope

import java.time.Duration

import carldata.borsuk.envelope.EnvelopeResultHashMapJsonProtocol.EnvelopeResultHashMapFormat
import carldata.borsuk.helper.JsonHelper.{doubleFromValue, stringFromValue}
import spray.json.{DefaultJsonProtocol, JsArray, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, _}

import scala.collection.immutable
import scala.collection.immutable.HashMap

class EnvelopeFileContent(er: immutable.HashMap[String, EnvelopeResult], bn: Int) {
  val envelopeResults: HashMap[String, EnvelopeResult] = er
  val buildNumber: Int = bn
}


object EnvelopeFileContentJsonProtocol extends DefaultJsonProtocol {

  implicit object EnvelopeFileContentFormat extends RootJsonFormat[EnvelopeFileContent] {
    def read(json: JsValue): EnvelopeFileContent = {
      json match {
        case JsObject(fields) =>
          new EnvelopeFileContent(fields("envelopes").convertTo[immutable.HashMap[String, EnvelopeResult]](EnvelopeResultHashMapJsonProtocol.EnvelopeResultHashMapFormat)
            , doubleFromValue(fields("build")).toInt)

        case _ => new EnvelopeFileContent(HashMap.empty, 0)
      }
    }

    def write(obj: EnvelopeFileContent): JsValue = {
      JsObject(
        "envelopes" -> obj.envelopeResults.toJson(EnvelopeResultHashMapFormat),
        "build" -> JsNumber(obj.buildNumber)
      )
    }
  }

}


object EnvelopeResultHashMapJsonProtocol extends DefaultJsonProtocol {

  import EnvelopeResultJsonProtocol._

  implicit object EnvelopeResultHashMapFormat extends RootJsonFormat[immutable.HashMap[String, EnvelopeResult]] {

    def read(json: JsValue): HashMap[String, EnvelopeResult] = {
      json match {
        case JsArray(elements) =>
          val pairs = elements.map {
            case JsObject(fields) => (
              stringFromValue(fields("key")),
              fields("value").convertTo[EnvelopeResult])
            case _ => ("", new EnvelopeResult(Seq(), Seq(), Duration.ZERO))
          }.toMap

          val hash = immutable.HashMap.empty
          hash.++(pairs)
        case _ => immutable.HashMap.empty[String, EnvelopeResult]
      }
    }

    def write(obj: HashMap[String, EnvelopeResult]): JsValue = {
      JsArray(obj.map(x => JsObject(
        "key" -> JsString(x._1),
        "value" -> x._2.toJson
      )).toVector)
    }
  }

}
