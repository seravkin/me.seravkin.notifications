package me.seravkin.notifications.domain.parsing

import java.time.Duration
import java.time.format.DateTimeFormatter

import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers { this: TimeConstants =>

  private[this] val FullDateRegex = """([0-9]{1,2})\.([0-9]{2})\.([0-9]{4})""".r
  private[this] val DateRegex = """([0-9]{1,2})\.([0-9]{2})""".r
  private[this] val TimeRegex = """([0-9]{1,2})\:([0-9]{2})""".r

  private[this] def numeral(value: Int, strings: String*): Parser[Int] =
    strings.map(literal).reduce { _ | _ } ^^ { _ => value }

  private[this] def alternativesOf[T](parsers: Parser[T]*): Parser[T] =
    parsers.reduce { _ | _ }

  def int: Parser[Int] = stringInt |  "([0-9]+)".r ^^ { _.toInt }

  def stringInt: Parser[Int] = alternativesOf(
    numeral(0, "ноль"),
    numeral(1, "один", "одну"),
    numeral(2, "два", "две"),
    numeral(3, "три"),
    numeral(4, "четыре"),
    numeral(5, "пять"),
    numeral(6, "шесть"),
    numeral(7, "семь"),
    numeral(8, "восемь"),
    numeral(9, "девять")
  )

  def manyDaysOfWeek: Parser[Set[Int]] = rep1sep(daysOfWeek, ",") ^^ { _.toSet }

  def workingDays: Parser[Set[Int]] = ("будний день" | "рабочий день" | "будний" |  "будни") ^^
    { _ => Set(0,1,2,3,4) }

  def weekends: Parser[Set[Int]] = ("выходные" | "выходной день" | "выходной") ^^
    { _ => Set(5, 6) }

  def days: Parser[Set[Int]] = weekends | workingDays | manyDaysOfWeek

  def timeUnit: Parser[Int => Duration] = second | minute | hour | day

  def duration: Parser[Duration] = int ~ timeUnit ^^ { case i ~ toDuration => toDuration(i) }

  def durations: Parser[MomentInFuture] = rep1(duration) ^^ { seq => FromDuration(seq.reduce(_ plus _)) }

  def validatedDateWithYear: Parser[(Int, Int, Int)] =
    FullDateRegex ^^ { case FullDateRegex(day, month, year) => (day.toInt, month.toInt, year.toInt) } filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  def validatedTime: Parser[(Int, Int)] =
    TimeRegex ^^ { case TimeRegex(hour, minutes) => hour.toInt -> minutes.toInt } filter(x => 0 <= x._1 && x._1 <= 23 && 0 <= x._2 && x._2 <= 59)

  def validatedDate: Parser[(Int, Int)] =
    DateRegex ^^ { case DateRegex(day, month) => day.toInt -> month.toInt } filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  def formattedDateWithYear: Parser[Date] = validatedDateWithYear ^^ { case (day, month, year) => FormattedDateWithYear(day, month, year) }

  def formattedDate: Parser[Date] = validatedDate ^^ { case (day, month) => FormattedDate(day, month) }

  def formattedTime: Parser[FormattedTime] = validatedTime ^^ { case (hour, minutes) => FormattedTime(hour, minutes) }

}
