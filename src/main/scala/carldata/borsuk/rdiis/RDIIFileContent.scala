package carldata.borsuk.rdiis

import carldata.borsuk.helper.JsonHelper.doubleFromValue
import carldata.borsuk.rdiis.RDIIObjectHashMapJsonProtocol.RDIIObjectHashMapFormat
import spray.json._

import scala.collection.immutable
import scala.collection.immutable.HashMap

class RDIIFileContent(er: immutable.HashMap[String, RDIIObject], bn: Int) {
  val rdiiResults: immutable.HashMap[String, RDIIObject] = er
  val buildNumber: Int = bn
}

object EnvelopeFileContentJsonProtocol extends DefaultJsonProtocol {

  implicit object EnvelopeFileContentFormat extends RootJsonFormat[RDIIFileContent] {
    def read(json: JsValue): RDIIFileContent = {
      json match {
        case JsObject(fields) =>
          new RDIIFileContent(fields("rdiis").convertTo[immutable.HashMap[String, RDIIObject]](RDIIObjectHashMapFormat)
            , doubleFromValue(fields("build")).toInt)

        case _ => new RDIIFileContent(HashMap.empty, 0)
      }
    }

    def write(obj: RDIIFileContent): JsValue = {
      JsObject(
        "rdiis" -> obj.rdiiResults.toJson(RDIIObjectHashMapFormat),
        "build" -> JsNumber(obj.buildNumber)
      )
    }
  }

}