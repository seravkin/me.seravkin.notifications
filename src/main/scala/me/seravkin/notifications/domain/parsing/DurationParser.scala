package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Internationalization
import me.seravkin.notifications.domain.internationalization.Words.ForUserString

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

class DurationParser(i8ln: Internationalization) extends RegexParsers with TimeConstants with CommonParsers with DateParsers with TimeParsers with RecurrentParsers with HasInternationalization {

  override def internationalization: Internationalization = i8ln

  private[this] val usernameParser = """[^\s]+""".r

  private[this] def caseInsensitive[T](p: => Parser[T]): Parser[T] = (in: Input) =>
    parseAll(p, in.source.toString.drop(in.offset).toLowerCase)

  def syntax: Parser[NotificationProgram] = (inTime | durations | recurrent) ^^ (time => time)

  def username: Parser[String] = anyOf(ForUserString) ~ "@"  ~ usernameParser ^^ { case  _ ~ _ ~ name => name }

  def withUsername: Parser[NotificationProgram] = (username ~ caseInsensitive(syntax)) ^^ { case name ~ s => ForUser(name, s) }

  def parse(string: String): Either[String, NotificationProgram] = {
    parseAll(withUsername | caseInsensitive(syntax), string) match {
      case Success(t, _) => Right(t)
      case Failure(msg, _) => Left(msg)
      case Error(msg, _) => Left(msg)
      case NoSuccess(msg, _) => Left(msg)
    }
  }

}
