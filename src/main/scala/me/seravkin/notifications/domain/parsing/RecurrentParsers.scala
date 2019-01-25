package me.seravkin.notifications.domain.parsing

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words.{Every, EveryDayIn, In}
import me.seravkin.notifications.domain.ast.RecurrentAst
import me.seravkin.notifications.domain.parsing.syntax.all._

final class RecurrentParsers[T, R](internalizationParsers: InternalizationParsers,
                                   commonParsers: CommonParsers[T],
                                   recurrentAst: RecurrentAst[R]) {
  import internalizationParsers._
  import commonParsers._
  def everyday: Parser[R] = anyOf(EveryDayIn) ~~ validatedTime -|
    { case (_,pair) => recurrentAst.inTime(pair._1, pair._2) }

  def everyDayOfWeek: Parser[R] = anyOf(Every) ~~ days ~~ anyOf(In) ~~ validatedTime -|
    { case (((_,d),_),pair) =>
      recurrentAst.everyDayOfWeek(d, recurrentAst.inTime(pair._1, pair._2)) }

  def recurrent: Parser[R] = everyday | everyDayOfWeek

}
