package carldata.borsuk.model

import carldata.borsuk.FileStorage
import spray.json._

class TimeSeriesModel(name: String, storage: FileStorage) extends Model {

  override def addSample(str: String): Unit = {
    str.parseJson match {
      case JsObject(fields) =>
        val index = stringFromValue(fields("index"))
        val value = stringFromValue(fields("value"))
        storage.add(name, index + "," + value)
      case _ =>
    }
  }

  private def stringFromValue(jsVal: JsValue): String = jsVal match {
    case JsString(str) => str
    case v: JsValue => v.toString
  }

}
