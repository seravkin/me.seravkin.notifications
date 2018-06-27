package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words.{Every, EveryDayIn, In}

import scala.util.parsing.combinator.RegexParsers

trait RecurrentParsers { this: TimeConstants with CommonParsers with RegexParsers with HasInternationalization =>

  def everyday: Parser[Recurrent] = anyOf(EveryDayIn) ~ formattedTime ^^
    { case _ ~ FormattedTime(hours, minutes) => InTime(hours, minutes) }

  def everyDayOfWeek: Parser[Recurrent] = anyOf(Every) ~ days ~ anyOf(In) ~ formattedTime ^^
    { case _ ~ d ~ _ ~ FormattedTime(hours, minutes) => EveryDaysOfWeek(d, InTime(hours, minutes)) }

  def recurrent: Parser[Recurrent] = everyday | everyDayOfWeek

}
