package carldata.borsuk.helper

import java.time.LocalDateTime

import spray.json.{JsArray, JsNumber, JsString, JsValue}
import carldata.borsuk.helper.DateTimeHelper._
object JsonHelper {

  def parseJsField(js: JsValue, str: String): String = js.asJsObject.fields(str).toString.replace("\"", "")

  def arrayFromValue(jsVal: JsValue): Array[Double] = jsVal match {
    case JsArray(vs) => vs.map(doubleFromValue).toArray
    case _ => Array.empty[Double]
  }

  def stringFromValue(jsVal: JsValue): String = jsVal match {
    case JsString(str) => str
    case v: JsValue => v.toString
  }

  def timestampFromValue(jsVal: JsValue): LocalDateTime = jsVal match {
    case JsString(str) =>
      try {
        dateParse(str)
      } catch {
        case _: Exception =>
          LocalDateTime.now()
      }
    case _ => LocalDateTime.now()
  }

  def doubleFromValue(jsVal: JsValue): Double = jsVal match {
    case JsNumber(v) => v.toDouble
    case _ => Double.NaN
  }

  def intFromValue(jsVal: JsValue): Int = jsVal match {
    case JsNumber(v) => v.toInt
    case _ => Int.MinValue
  }
}
