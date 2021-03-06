package me.seravkin.notifications.domain.parsing

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.ast.RelativeAst
import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.parsing.syntax.all._

final class RelativeParsers[T](internalizationParsers: InternalizationParsers,
                               commonParsers: CommonParsers[T],
                               timeConstants: TimeConstants,
                               relativeAst: RelativeAst[T]) {
  import internalizationParsers._
  import commonParsers._
  import timeConstants._

  def today: Parser[T] = anyOf(Today) -| { _ => relativeAst.inDays(0) }

  def tomorrow: Parser[T] = anyOf(Tomorrow) -| { _ => relativeAst.inDays(1) }

  def dayAfterTomorrow: Parser[T] = anyOf(DayAfterTomorrow) -| { _ => relativeAst.inDays(2) }

  def inDays: Parser[T] = (anyOf(InAsInTime) ~~ int ~~ day) -| { case ((_,i),_) => relativeAst.inDays(i)}

  def inWeek: Parser[Int] = anyOf(InWeek) -| { _ => 1 }

  def inWeeks: Parser[Int] = (anyOf(InAsInTime) ~~ int ~~ week) -| { case ((_,i),_) => i }

  def inWeeksAsDays: Parser[T] = (inWeek | inWeeks) -| { i => relativeAst.inDays(i * 7) }

  def dayOfWeekInWeeks: Parser[T] = anyOf(In) ~~ daysOfWeek ~ Atto.optWs(Atto.ws1 ~> (inWeek | inWeeks)) -| { case ((_,dayOf),weeks) =>
    relativeAst.dayOfWeek(weeks.getOrElse(0), dayOf) }

  def date: Parser[T] = formattedDateWithYear | formattedDate | dateWithMonthAndYear | dateWithMonth |
    today | tomorrow | dayAfterTomorrow | inDays | inWeeksAsDays | dayOfWeekInWeeks

}
