package me.seravkin.notifications.domain.parsing

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words.{CurrentTime, In}
import me.seravkin.notifications.domain.parsing.syntax.all._

final class TimeParsers[T](internalizationParsers: InternalizationParsers,
                           timeConstants: TimeConstants,
                           dateParsers: DateParsers[T],
                           commonParsers: CommonParsers[T],
                           momentInFutureAst: MomentInFutureAst[T]) {
  
  import internalizationParsers._
  import timeConstants._
  import dateParsers._
  import commonParsers._
  
  def inSameTime: Parser[T] = anyOf(CurrentTime) -| { _ => momentInFutureAst.inCurrentTime }

  def time: Parser[T] = formattedTime | inSameTime

  def timePeriods: Parser[T] = periods -| { momentInFutureAst.fuzzyTime }

  def inAndTime(timeParser: Parser[T]): Parser[T] = formattedTime | timePeriods | (anyOf(In) ~~ timeParser -| { case (_, t) => t })

  def inTime(timeParser: Parser[T]) : Parser[T] = (Atto.optWs(date <~ Atto.ws1) ~ inAndTime(timeParser)) -|  { case (day, time) =>
    momentInFutureAst.dateAndTime(day.getOrElse(momentInFutureAst.inDays(0)), time) }

}
