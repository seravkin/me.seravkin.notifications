package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words.{CurrentTime, In}

import scala.util.parsing.combinator.RegexParsers

trait TimeParsers[T] { this: CommonParsers[T] with TimeConstants with DateParsers[T] with RegexParsers with HasInternationalization with HasMomentInFutureAst[T] =>

  def inSameTime: Parser[T] = anyOf(CurrentTime) ^^ { _ => momentInFutureAst.inCurrentTime }

  def time: Parser[T] = formattedTime | inSameTime

  def timePeriods: Parser[T] = periods ^^ { momentInFutureAst.fuzzyTime }

  def inAndTime(timeParser: Parser[T]): Parser[T] = formattedTime | timePeriods | (anyOf(In) ~ timeParser ^^ { case _ ~ t => t })

  def inTime(timeParser: Parser[T]) : Parser[T] = (date.? ~ inAndTime(timeParser)) ^^  { case day ~ time =>
    momentInFutureAst.dateAndTime(day.getOrElse(momentInFutureAst.inDays(0)), time) }

}
