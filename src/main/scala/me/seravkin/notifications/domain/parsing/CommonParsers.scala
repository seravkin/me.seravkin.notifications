package me.seravkin.notifications.domain.parsing

import java.time.Duration

import me.seravkin.notifications.domain.internationalization.Words._

import scala.util.parsing.combinator._

trait CommonParsers[T] extends RegexParsers { this: TimeConstants with HasInternationalization with HasMomentInFutureAst[T] =>

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

  def durations: Parser[T] = rep1(duration) ^^ { seq => momentInFutureAst.duration(seq.reduce(_ plus _)) }

  def validatedDateWithYear: Parser[(Int, Int, Int)] =
    FullDateRegex ^^ { case FullDateRegex(day, month, year) => (day.toInt, month.toInt, year.toInt) } filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  private def isHours(x: Int): Boolean = 0 <= x && x <= 23

  def validatedTime: Parser[(Int, Int)] =
    TimeRegex ^^ { case TimeRegex(hour, minutes) => hour.toInt -> minutes.toInt } filter(x => isHours(x._1) && 0 <= x._2 && x._2 <= 59)

  def validatedDate: Parser[(Int, Int)] =
    DateRegex ^^ { case DateRegex(day, month) => day.toInt -> month.toInt } filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  def formattedDateWithYear: Parser[T] = validatedDateWithYear ^^ { case (day, month, year) =>
    momentInFutureAst.date(day, month, year) }

  def validatedHour: Parser[T] =
    int.filter(isHours) ^^ { hour => momentInFutureAst.time(hour, 0) }

  def formattedDate: Parser[T] = validatedDate ^^ { case (day, month) =>
    momentInFutureAst.date(day, month) }

  def validatedDay: Parser[Int] =
    int filter { x => 1 <= x && x <= 31 }

  def dateWithMonth: Parser[T] =
    validatedDay ~ months ^^ { case day ~ month =>
      momentInFutureAst.date(day, month.toInt)
    }

  def dateWithMonthAndYear: Parser[T] =
    validatedDay ~ months ~ int ^^ { case day ~ month ~ year =>
    momentInFutureAst.date(day, month.toInt, year) }

  def formattedTime: Parser[T] = validatedTime ^^ { case (hour, minutes) =>
    momentInFutureAst.time(hour, minutes) }

}
