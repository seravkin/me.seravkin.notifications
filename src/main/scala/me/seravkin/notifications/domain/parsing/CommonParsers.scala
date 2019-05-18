package me.seravkin.notifications.domain.parsing

import java.time.Duration

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.ast.{DateAst, DurationAst, TimeAst}
import me.seravkin.notifications.domain.parsing.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words._

final class CommonParsers[T](internalizationParsers: InternalizationParsers,
                             timeConstants: TimeConstants,
                             durationAst: DurationAst[T],
                             dateAst: DateAst[T],
                             timeAst: TimeAst[T]) {

  import internalizationParsers._
  import timeConstants._


  private[this] def numeral(value: Int): Parser[Int] =
    anyOf(Numeral(value)) -| { _ => value }

  def int: Parser[Int] = stringInt |  Atto.int

  def stringInt: Parser[Int] = 0.to(9).map(numeral).reduce { _ | _ }

  def manyDaysOfMonth: Parser[Set[Int]] = int.filter(_ <= 28).sepBy1(Atto.ws ~> Atto.string(",") <~ Atto.ws) -|
    { _.toList.toSet }

  def manyDaysOfWeek: Parser[Set[Int]] = daysOfWeek
    .sepBy1(Atto.ws ~> Atto.string(",") <~ Atto.ws) -| { _.toList.toSet }

  def workingDays: Parser[Set[Int]] = anyOf(WorkingDay) -|
    { _ => Set(0,1,2,3,4) }

  def weekends: Parser[Set[Int]] = anyOf(Weekend) -|
    { _ => Set(5, 6) }

  def days: Parser[Set[Int]] = weekends | workingDays | manyDaysOfWeek

  def timeUnit: Parser[Int => Duration] = second | minute | hour | day

  def duration: Parser[Duration] = int ~~ timeUnit -| { case (i, toDuration) => toDuration(i) }

  def durations: Parser[T] = duration.manyWs1 -| { seq => durationAst.duration(seq.toList.reduce(_ plus _)) }

  def validatedDateWithYear: Parser[(Int, Int, Int)] =
    fullDateRegex filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  private def isHours(x: Int): Boolean = 0 <= x && x <= 23

  def validatedTime: Parser[(Int, Int)] =
    timeRegex filter(x => isHours(x._1) && 0 <= x._2 && x._2 <= 59)

  def validatedDate: Parser[(Int, Int)] =
    dateRegex filter(x => 1 <= x._1 && x._1 <= 31 && 1 <= x._2 && x._2 <= 12 )

  def formattedDateWithYear: Parser[T] = validatedDateWithYear -| { case (day, month, year) =>
    dateAst.date(day, month, year) }

  def validatedHour: Parser[T] =
    int.filter(isHours) -| { hour => timeAst.time(hour, 0) }

  def formattedDate: Parser[T] = validatedDate -| { case (day, month) =>
    dateAst.date(day, month) }

  def validatedDay: Parser[Int] =
    int filter { x => 1 <= x && x <= 31 }

  def dateWithMonth: Parser[T] =
    validatedDay ~~ months -| { case (day, month) =>
      dateAst.date(day, month.toInt)
    }

  def dateWithMonthAndYear: Parser[T] =
    validatedDay ~~ months ~~ int -| { case ((day, month), year) =>
    dateAst.date(day, month.toInt, year) }

  def formattedTime: Parser[T] = validatedTime -| { case (hour, minutes) =>
    timeAst.time(hour, minutes) }

}
