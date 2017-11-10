package carldata.borsuk

import spray.json.{JsNumber, JsString, JsValue}

/**
  * Json helper
  */
object Jsons {

  /** Convert value to string */
  def stringFromValue(jsVal: JsValue): String = jsVal match {
    case JsString(str) => str
    case v: JsValue => v.toString
  }

  /** Convert value to float */
  def floatFromValue(jsVal: JsValue): Float = jsVal match {
    case JsNumber(v) => v.floatValue()
    case _ => 0f
  }

}
