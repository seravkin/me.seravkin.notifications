package me.seravkin.notifications.domain.parsing

import scala.util.parsing.combinator.RegexParsers

trait DateParsers extends RegexParsers { this: CommonParsers with TimeConstants =>

  def today: Parser[Date] = ("сегодня" | "today") ^^ { _ => InDays(0) }

  def tomorrow: Parser[Date] = ("завтра" | "tomorrow") ^^ { _ => InDays(1) }

  def dayAfterTomorrow: Parser[Date] = ("послезавтра" | "day after tomorrow") ^^ { _ => InDays(2) }

  def inDays: Parser[Date] = ("через" ~ int ~ day) ^^ { case _ ~ i ~ _ => InDays(i)}

  def inWeek: Parser[Int] = ("через" ~ "неделю") ^^ { _ => 1 }

  def inWeeks: Parser[Int] = ("через" ~ int ~ week) ^^ { case _ ~ i ~ _ => i }

  def inWeeksAsDays: Parser[Date] = (inWeek | inWeeks) ^^ { i => InDays(i * 7) }

  def dayOfWeekInWeeks: Parser[Date] = ("во" | "в") ~ daysOfWeek ~ (inWeek | inWeeks).? ^^ { case _ ~ dayOf ~ weeks => InNextDayOfWeek(weeks.getOrElse(0), dayOf) }

  def date: Parser[Date] = formattedDateWithYear | formattedDate | today | tomorrow | dayAfterTomorrow | inDays | inWeeksAsDays | dayOfWeekInWeeks

}
