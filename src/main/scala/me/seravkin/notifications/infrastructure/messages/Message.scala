package me.seravkin.notifications.infrastructure.messages

import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait Message[Msg] {
  def username(msg: Msg): Option[String]

  def text(msg: Msg): Option[String]

  def chatId(msg: Msg): Long

  def isPrivate(msg: Msg): Boolean

  def data(msg: Msg): Option[String]
}

object Message {
  implicit class MessageOps[Msg](msg: Msg)(implicit ev: Message[Msg]) {
    def username: Option[String] = ev.username(msg)
    def text: Option[String] = ev.text(msg)
    def chatId: Long = ev.chatId(msg)
    def data: Option[String] = ev.data(msg)
    def isPrivate: Boolean = ev.isPrivate(msg)
  }

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

  object ContainsData {
    def unapply[Msg: Message](arg: Msg): Option[String] =
      arg.data
  }

  object ContainsText {
    def unapply[Msg: Message](arg: Msg): Option[String] =
      arg.text
  }

  object IsLong {
    def unapply(arg: String): Option[Long] =
      Try(arg.toLong).toOption
  }
}