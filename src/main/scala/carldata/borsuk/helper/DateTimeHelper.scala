package carldata.borsuk.helper

import java.time._
import java.time.format.DateTimeFormatterBuilder
import java.time.temporal.ChronoField

/**
  * Created by Krzysztof Langner on 2018-03-21.
  */
object DateTimeHelper {

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

    LocalDateTime.parse(str.replaceAll("\"", ""), formatter)
  }

  def instantToLDT(dt: Instant): LocalDateTime = {
    LocalDateTime.ofInstant(dt, ZoneOffset.UTC)
  }

  def isDateHoliday(dt: LocalDate): Boolean = {
    dt.getDayOfWeek.getValue == 6 || dt.getDayOfWeek.getValue == 7
  }

  def isHoliday(dt: Instant): Boolean = {
    val day = instantToLDT(dt).toLocalDate
    day.getDayOfWeek.getValue == 6 || day.getDayOfWeek.getValue == 7
  }

  def dayToInstant(dt: LocalDate): Instant = {
    Instant.ofEpochSecond(dt.toEpochDay * 86400)
  }

  def dtToInstant(dt: LocalDateTime): Instant = {
    dt.toInstant(ZoneOffset.UTC)
  }

  def instantToDay(dt: Instant): LocalDate = {
    LocalDate.ofEpochDay(dt.getEpochSecond / 86400)
  }

  def instantToTime(dt: Instant): LocalTime = {
    instantToLDT(dt).toLocalTime
  }


}
