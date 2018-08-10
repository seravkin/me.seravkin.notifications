package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Internationalization
import me.seravkin.notifications.domain.internationalization.Words._

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

final class DurationParser[T](i8ln: Internationalization, futureAst: MomentInFutureAst[T], recAst: RecurrentAst[T])
  extends RegexParsers with TimeConstants with CommonParsers[T]
  with DateParsers[T] with TimeParsers[T]
  with HasInternationalization with HasMomentInFutureAst[T] with HasRecurrentAst[T] with RecurrentParsers[T, T] {

  override def internationalization: Internationalization = i8ln
  override def momentInFutureAst: MomentInFutureAst[T] = futureAst
  override def recurrentAst: RecurrentAst[T] = recAst

  private[this] val usernameParser = """[^\s]+""".r

  private[this] def caseInsensitive[K](p: => Parser[K]): Parser[K] = (in: Input) =>
    parseAll(p, in.source.toString.drop(in.offset).toLowerCase)

  def simpleDuration: Parser[T] = inTime | durations

  def durationWithConfirmation: Parser[T] = simpleDuration ~ anyOf(With) ~ anyOf(Confirmation) ~ (anyOf(Every) ~ duration).? ^^ {
    case dur ~ _ ~ _ ~ period =>
      momentInFutureAst.confirmation(period.map(_._2), dur)
  }

  def syntax: Parser[T] = (recurrent | durationWithConfirmation | simpleDuration) ^^ (time => time)

  def username: Parser[String] = anyOf(ForUserString) ~ "@"  ~ usernameParser ^^ { case  _ ~ _ ~ name => name }

  def withUsername: Parser[T] = (username ~ caseInsensitive(syntax)) ^^ { case name ~ s =>
    momentInFutureAst.forUser(name, s) }

  def parse(string: String): Either[String, T] = parseAll(withUsername | caseInsensitive(syntax), string) match {
      case Success(t, _) => Right(t)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
      case NoSuccess(msg, _) => Left(msg)
    }


}
