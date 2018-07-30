package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Internationalization
import me.seravkin.notifications.domain.internationalization.Words.ForUserString

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

class DurationParser[T](i8ln: Internationalization, futureAst: MomentInFutureAst[T])
  extends RegexParsers with TimeConstants with CommonParsers[T]
  with DateParsers[T] with TimeParsers[T]
  with HasInternationalization with HasMomentInFutureAst[T] {

  override def internationalization: Internationalization = i8ln

  override def momentInFutureAst: MomentInFutureAst[T] = futureAst

  private[this] val usernameParser = """[^\s]+""".r

  private[this] def caseInsensitive[K](p: => Parser[K]): Parser[K] = (in: Input) =>
    parseAll(p, in.source.toString.drop(in.offset).toLowerCase)

  def syntax: Parser[T] = (inTime | durations) ^^ (time => time)

  def username: Parser[String] = anyOf(ForUserString) ~ "@"  ~ usernameParser ^^ { case  _ ~ _ ~ name => name }

  def withUsername: Parser[T] = (username ~ caseInsensitive(syntax)) ^^ { case name ~ s =>
    momentInFutureAst.forUser(name, s) }

  def parse(string: String): Either[String, T] = {
    parseAll(withUsername | caseInsensitive(syntax), string) match {
      case Success(t, _) => Right(t)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
      case NoSuccess(msg, _) => Left(msg)
    }
  }

}
