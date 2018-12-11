package me.seravkin.notifications.domain.parsing

import atto.Parser.{Failure, State, Success, TResult}
import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.parsing.syntax.all._

final class DurationParser[T](internalizationParsers: InternalizationParsers,
                              commonParsers: CommonParsers[T],
                              timeParsers: TimeParsers[T],
                              recurrentParsers: RecurrentParsers[T, T],
                              momentInFutureAst: MomentInFutureAst[T],
                              recAst: RecurrentAst[T]) {

  import internalizationParsers._
  import commonParsers._
  import timeParsers._
  import recurrentParsers._

  private[this] val usernameParser = Atto.char('@') ~> Atto.stringOf1(Atto.notChar(' '))

  private[this] def caseInsensitive[K](p: => Parser[K]): Parser[K] =
    new Parser[K] {
      override def toString = "caseInsensitive"
      def apply[R](st0: State, kf: Failure[R], ks: Success[K,R]): TResult[R] =
        p(st0.copy(input = st0.input.toLowerCase), kf, ks)
    }

  def simpleDuration: Parser[T] = inTime(time) | durations | inTime(validatedHour)

  def durationWithConfirmation: Parser[T] = simpleDuration ~~ anyOf(With) ~~ anyOf(Confirmation) ~
    Atto.optWs(Atto.whitespace ~ anyOf(Every) ~~ duration) -| {
    case (((dur,_),_), period) =>
      momentInFutureAst.confirmation(period.map(_._2), dur)
  }

  def syntax: Parser[T] = recurrent | durationWithConfirmation | simpleDuration

  def username: Parser[String] = anyOf(ForUserString) ~~ usernameParser -| {
    case (_, name) => name
  }

  def withUsername: Parser[T] = (username ~~ caseInsensitive(syntax)) -| { case (name, s) =>
    momentInFutureAst.forUser(name, s) }

  def parse(string: String): Either[String, T] = (withUsername | caseInsensitive(syntax))
    .parseOnly(string)
    .either
}