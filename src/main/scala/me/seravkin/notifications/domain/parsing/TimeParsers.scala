package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words.{CurrentTime, In}

import scala.util.parsing.combinator.RegexParsers

trait TimeParsers { this: CommonParsers with TimeConstants with DateParsers with RegexParsers with HasInternationalization =>

  def inSameTime: Parser[Time] = anyOf(CurrentTime) ^^ { _ => InCurrentTime }

  def time: Parser[Time] = formattedTime | inSameTime

  def timePeriods: Parser[Time] = periods ^^ { AtFuzzyTime }

  def inAndTime: Parser[Time] = formattedTime | timePeriods | (anyOf(In) ~ time ^^ { case _ ~ t => t })

  def inTime: Parser[MomentInFuture] = (date.? ~ inAndTime) ^^  { case day ~ time => FromFormattedDate(day.getOrElse(InDays(0)), time) }

}
