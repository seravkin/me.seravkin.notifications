package me.seravkin.notifications.infrastructure.messages

import scala.util.Try
import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.parsing.syntax.all._

object Message {

  private object ArgumentParser {

    private[this] val stringInQuotes: Parser[String] =
      Atto.char('\"') ~> Atto.stringOf1(Atto.noneOf("\"")) <~ Atto.char('\"')

    private[this] val anyString: Parser[String] =
      Atto.stringOf1(Atto.noneOf(" "))

    private[this] val arguments: Parser[List[String]] = (stringInQuotes | anyString).manyWs1 -| { _.toList }

    def parse(string: String): Option[List[String]] = {
      val result =
        arguments.parseOnly(string)

      result.option
    }
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