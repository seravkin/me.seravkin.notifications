package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words._

import scala.util.parsing.combinator.RegexParsers

trait DateParsers extends RegexParsers { this: CommonParsers with TimeConstants with HasInternationalization =>

  def today: Parser[Date] = anyOf(Today) ^^ { _ => InDays(0) }

  def tomorrow: Parser[Date] = anyOf(Tomorrow) ^^ { _ => InDays(1) }

  def dayAfterTomorrow: Parser[Date] = anyOf(DayAfterTomorrow) ^^ { _ => InDays(2) }

  def inDays: Parser[Date] = (anyOf(InAsInTime) ~ int ~ day) ^^ { case _ ~ i ~ _ => InDays(i)}

  def inWeek: Parser[Int] = anyOf(InWeek) ^^ { _ => 1 }

  def inWeeks: Parser[Int] = (anyOf(InAsInTime) ~ int ~ week) ^^ { case _ ~ i ~ _ => i }

  def inWeeksAsDays: Parser[Date] = (inWeek | inWeeks) ^^ { i => InDays(i * 7) }

  def dayOfWeekInWeeks: Parser[Date] = anyOf(In) ~ daysOfWeek ~ (inWeek | inWeeks).? ^^ { case _ ~ dayOf ~ weeks => InNextDayOfWeek(weeks.getOrElse(0), dayOf) }

  def date: Parser[Date] = formattedDateWithYear | formattedDate | today | tomorrow | dayAfterTomorrow | inDays | inWeeksAsDays | dayOfWeekInWeeks

}
