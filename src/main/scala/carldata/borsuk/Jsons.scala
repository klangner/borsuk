package carldata.borsuk

import spray.json.{JsString, JsValue}

/**
  * Json helper
  */
object Jsons {

  /** Convert value to string */
  def stringFromValue(jsVal: JsValue): String = jsVal match {
    case JsString(str) => str
    case v: JsValue => v.toString
  }

}
