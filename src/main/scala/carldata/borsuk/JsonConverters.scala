package carldata.borsuk

import java.time.LocalDateTime
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

import spray.json.{JsArray, JsNumber, JsString, JsValue}

/**
  * Custom function for simplify json access
  */
object JsonConverters {

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

  def floatFromValue(jsVal: JsValue): Float = jsVal match {
    case JsNumber(v) => v.toFloat
    case _ => Float.NaN
  }

  def textFromLines(jsVal: JsValue): String = jsVal match {
    case JsArray(vs) => vs.map(stringFromValue).mkString("\n")
    case _ => ""
  }

  def dateParse(str: String): LocalDateTime = {
    val formatter = new DateTimeFormatterBuilder()
      .parseCaseInsensitive
      .appendValue(ChronoField.YEAR)
      .appendLiteral('-')
      .appendValue(ChronoField.MONTH_OF_YEAR)
      .appendLiteral('-')
      .appendValue(ChronoField.DAY_OF_MONTH)
      .optionalStart.appendLiteral(' ').optionalEnd
      .optionalStart.appendLiteral('T').optionalEnd
      .optionalStart
      .appendValue(ChronoField.HOUR_OF_DAY)
      .appendLiteral(':')
      .appendValue(ChronoField.MINUTE_OF_HOUR)
      .optionalStart.appendLiteral(':').appendValue(ChronoField.SECOND_OF_MINUTE).optionalEnd
      .optionalStart.appendFraction(ChronoField.NANO_OF_SECOND, 0, 9, true).optionalEnd
      .optionalStart.appendLiteral('Z').optionalEnd
      .optionalEnd
      .parseDefaulting(ChronoField.HOUR_OF_DAY, 0)
      .parseDefaulting(ChronoField.MINUTE_OF_HOUR, 0)
      .parseDefaulting(ChronoField.SECOND_OF_MINUTE, 0)
      .parseDefaulting(ChronoField.NANO_OF_SECOND, 0)
      .toFormatter

    LocalDateTime.parse(str, formatter)
  }

  def arrayFromValue(jsVal: JsValue): Seq[String] = jsVal match {
    case JsArray(vs) => vs.map(stringFromValue)
    case _ => Seq()
  }

}
