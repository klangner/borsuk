package carldata.borsuk

import spray.json._

class TimeSeriesModel(storage: FileStorage) {

  def addSample(dataSet: String, str: String): Unit = {
    str.parseJson match {
      case JsObject(fields) => {
        val index = stringFromValue(fields("index"))
        val value = stringFromValue(fields("value"))
        storage.add(dataSet, index + "," + value)
      }
      case _ =>
    }
  }

  def stringFromValue(jsVal: JsValue): String = jsVal match {
    case JsString(str) => str
    case v: JsValue => v.toString
  }

}
