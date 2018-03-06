package me.seravkin.notifications.domain.parsing

import scala.util.parsing.combinator.RegexParsers

class DurationParser extends RegexParsers with TimeConstants with CommonParsers with DateParsers with TimeParsers with RecurrentParsers {

    private[this] val usernameParser = """[^\s]+""".r

    def syntax: Parser[NotificationProgram] = (inTime | durations | recurrent) ^^ (time => time)

    def withUsername: Parser[NotificationProgram] = (("для @" | "for @" | "@") ~> usernameParser ~ syntax) ^^ { case name ~ s => ForUser(name, s) }

    def parse(string: String) : Either[String, NotificationProgram] = {
      parseAll(withUsername | syntax, string) match {
        case Success(t, _) => Right(t)
        case Failure(msg, _) => Left(msg)
        case Error(msg, _) => Left(msg)
        case NoSuccess(msg, _) => Left(msg)
      }
    }

  }
