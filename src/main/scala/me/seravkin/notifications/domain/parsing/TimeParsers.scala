package me.seravkin.notifications.domain.parsing

import scala.util.parsing.combinator.RegexParsers

trait TimeParsers extends RegexParsers { this: CommonParsers with TimeConstants with DateParsers =>

  def inSameTime: Parser[Time] = ("это же время" | "текущее время" | "current time") ^^ { _ => InCurrentTime }

  def time: Parser[Time] = formattedTime | inSameTime

  def inTime: Parser[MomentInFuture] = (date ~ "в" ~ time) ^^  { case day ~ _ ~ time => FromFormattedDate(day, time) }

}
