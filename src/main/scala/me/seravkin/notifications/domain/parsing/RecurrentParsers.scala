package me.seravkin.notifications.domain.parsing

import scala.util.parsing.combinator.RegexParsers

trait RecurrentParsers extends RegexParsers { this: TimeConstants with CommonParsers =>

  def everyday: Parser[Recurrent] = "каждый" ~ "день" ~ "в" ~ formattedTime ^^
    { case _ ~ _ ~ _ ~ FormattedTime(hours, minutes) => InTime(hours, minutes) }

  def everyDayOfWeek: Parser[Recurrent] = ("каждый" | "каждые" | "каждое" | "каждую") ~ days ~ "в" ~ formattedTime ^^
    { case _ ~ d ~ _ ~ FormattedTime(hours, minutes) => EveryDaysOfWeek(d, InTime(hours, minutes)) }

  def recurrent: Parser[Recurrent] = everyday | everyDayOfWeek

}
