package carldata.borsuk.helper

import java.time.{Duration, LocalDateTime}

import spray.json.{JsArray, JsNumber, JsString, JsValue}
import carldata.borsuk.helper.DateTimeHelper._

import scala.reflect.ClassTag

object JsonHelper {

  def parseJsField(js: JsValue, str: String): String = js.asJsObject.fields(str).toString.replace("\"", "")

  def arrayFromValue[T: ClassTag](jsVal: JsValue, f: JsValue => T): Array[T] = {
    jsVal match {
      case JsArray(vs) => vs.map(f).toArray
      case _ => Array.empty[T]
    }
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

  def durationFromValue(jsVal: JsValue): Duration = {
    jsVal match {
      case JsString(x) => Duration.parse(x)
      case _ => Duration.ZERO
    }
  }
}
