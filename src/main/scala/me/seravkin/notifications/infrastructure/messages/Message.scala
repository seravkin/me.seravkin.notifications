package me.seravkin.notifications.infrastructure.messages

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

object Message {

  private object ArgumentParser extends RegexParsers {

    private[this] val stringInQuotes: Parser[String] = "\"(.+)\"".r map { _.replace("\"", "") }
    private[this] val anyString: Parser[String] = """([^\s]+)""".r

    private[this] val arguments: Parser[List[String]] = rep(stringInQuotes | anyString)

    def parse(string: String): Option[List[String]] =
      parseAll(arguments, string).map(Some(_)).getOrElse(None)

  }

  object CommandWithArgs {
    def unapply(arg: String): Option[(String, List[String])] = arg match {
      case s if s.isEmpty => None
      case s =>
        val args = s.split(" ").toList
        Some(args.head -> args.tail)
    }
  }

  object CommandWithQuotedArgs {
    def unapply(arg: String): Option[(String, List[String])] = arg match {
      case s if s.isEmpty => None
      case s =>
        ArgumentParser.parse(s).map(l => l.head -> l.tail)
    }
  }

  object TailAsText {
    def unapply(arg: List[String]): Option[String] =
      if(arg.isEmpty)
        None
      else
        Some(arg.reduce(_ + " " + _))
  }

  object IsLong {
    def unapply(arg: String): Option[Long] =
      Try(arg.toLong).toOption
  }
}