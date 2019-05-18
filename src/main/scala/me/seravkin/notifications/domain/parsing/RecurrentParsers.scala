package me.seravkin.notifications.domain.parsing

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.ast.RecurrentAst
import me.seravkin.notifications.domain.parsing.syntax.all._

final class RecurrentParsers[T, R](internalizationParsers: InternalizationParsers,
                                   commonParsers: CommonParsers[T],
                                   recurrentAst: RecurrentAst[R]) {
  import internalizationParsers._
  import commonParsers._
  def everyday: Parser[R] = anyOf(EveryDayIn) ~~ validatedTime -|
    { case (_,pair) => recurrentAst.inTime(pair._1, pair._2) }

  def everyDayOfMonth: Parser[R] = anyOf(EveryMonthIn) ~~ int ~~ anyOf(Day) ~~ anyOf(In) ~~
    validatedTime -| {
    case ((((_, d),_),_),(h,m)) =>
      recurrentAst.everyDayOfMonth(Set(d - 1), recurrentAst.inTime(h, m))
  }

  def everyDayOfWeek: Parser[R] = anyOf(Every) ~~ days ~~ anyOf(In) ~~ validatedTime -|
    { case (((_,d),_),(h,m)) =>
      recurrentAst.everyDayOfWeek(d, recurrentAst.inTime(h, m)) }

  def recurrent: Parser[R] = everyday | everyDayOfWeek | everyDayOfMonth

}
