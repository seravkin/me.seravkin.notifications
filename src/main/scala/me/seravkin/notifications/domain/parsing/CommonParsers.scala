package me.seravkin.notifications.domain.parsing

import java.time.Duration
import java.time.format.DateTimeFormatter

import me.seravkin.notifications.domain.internationalization.Words._

import scala.util.parsing.combinator._

trait CommonParsers extends RegexParsers { this: TimeConstants with HasInternationalization =>

  private[this] val FullDateRegex = internationalization.words(FullDateRegexString).head.r
  private[this] val DateRegex = internationalization.words(DateRegexString).head.r
  private[this] val TimeRegex = internationalization.words(TimeRegexString).head.r

  private[this] def numeral(value: Int): Parser[Int] =
    anyOf(Numeral(value)) ^^ { _ => value }

  def int: Parser[Int] = stringInt |  "([0-9]+)".r ^^ { _.toInt }

  def stringInt: Parser[Int] = 0.to(9).map(numeral).reduce { _ | _ }

  def manyDaysOfWeek: Parser[Set[Int]] = rep1sep(daysOfWeek, ",") ^^ { _.toSet }

  def workingDays: Parser[Set[Int]] = anyOf(WorkingDay) ^^
    { _ => Set(0,1,2,3,4) }

  def weekends: Parser[Set[Int]] = anyOf(Weekend) ^^
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
