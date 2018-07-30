package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Words.{Every, EveryDayIn, In}

import scala.util.parsing.combinator.RegexParsers

trait RecurrentParsers[T, R] { this: TimeConstants with CommonParsers[T] with RegexParsers with HasInternationalization with HasRecurrentAst[R] =>

  def everyday: Parser[R] = anyOf(EveryDayIn) ~ validatedTime ^^
    { case _ ~ pair => recurrentAst.inTime(pair._1, pair._2) }

  def everyDayOfWeek: Parser[R] = anyOf(Every) ~ days ~ anyOf(In) ~ validatedTime ^^
    { case _ ~ d ~ _ ~ pair =>
      recurrentAst.everyDayOfWeek(d, recurrentAst.inTime(pair._1, pair._2)) }

  def recurrent: Parser[R] = everyday | everyDayOfWeek

}
