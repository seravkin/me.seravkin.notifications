package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words._

import scala.util.parsing.combinator.RegexParsers

trait DateParsers[T] extends RegexParsers { this: CommonParsers[T] with TimeConstants with HasInternationalization with HasMomentInFutureAst[T] =>

  def today: Parser[T] = anyOf(Today) ^^ { _ => momentInFutureAst.inDays(0) }

  def tomorrow: Parser[T] = anyOf(Tomorrow) ^^ { _ => momentInFutureAst.inDays(1) }

  def dayAfterTomorrow: Parser[T] = anyOf(DayAfterTomorrow) ^^ { _ => momentInFutureAst.inDays(2) }

  def inDays: Parser[T] = (anyOf(InAsInTime) ~ int ~ day) ^^ { case _ ~ i ~ _ => momentInFutureAst.inDays(i)}

  def inWeek: Parser[Int] = anyOf(InWeek) ^^ { _ => 1 }

  def inWeeks: Parser[Int] = (anyOf(InAsInTime) ~ int ~ week) ^^ { case _ ~ i ~ _ => i }

  def inWeeksAsDays: Parser[T] = (inWeek | inWeeks) ^^ { i => momentInFutureAst.inDays(i * 7) }

  def dayOfWeekInWeeks: Parser[T] = anyOf(In) ~ daysOfWeek ~ (inWeek | inWeeks).? ^^ { case _ ~ dayOf ~ weeks =>
    momentInFutureAst.dayOfWeek(weeks.getOrElse(0), dayOf) }

  def date: Parser[T] = formattedDateWithYear | formattedDate | today | tomorrow | dayAfterTomorrow | inDays | inWeeksAsDays | dayOfWeekInWeeks

}
