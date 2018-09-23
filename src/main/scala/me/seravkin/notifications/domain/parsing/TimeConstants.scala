package me.seravkin.notifications.domain.parsing

import java.time.Duration

import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.parsing.Period._

import scala.util.parsing.combinator.RegexParsers

trait TimeConstants { this: RegexParsers with HasInternationalization =>

  private[this] def word(word: Word, f: Long => Duration): Parser[Int => Duration] =
    anyOf(word) ^^ { _ => i: Int => f(i) }

  private[this] def dayOfWeek[T <: Word with DayOfWeek](t: T): Parser[Int] =
    anyOf(t) ^^ { _ => t.toInt }

  def second: Parser[Int => Duration] = word(Second, Duration.ofSeconds)
  def minute: Parser[Int => Duration] = word(Minute, Duration.ofMinutes)
  def hour: Parser[Int => Duration] = word(Hour, Duration.ofHours)
  def day: Parser[Int => Duration] = word(Day, Duration.ofDays)
  def week: Parser[Int => Duration] = word(Week, i => Duration.ofDays(i * 7))

  def monday: Parser[Int] = dayOfWeek(Monday)
  def tuesday: Parser[Int] = dayOfWeek(Tuesday)
  def wednesday: Parser[Int] = dayOfWeek(Wednesday)
  def thursday: Parser[Int] = dayOfWeek(Thursday)
  def friday: Parser[Int] = dayOfWeek(Friday)
  def saturday: Parser[Int] = dayOfWeek(Saturday)
  def sunday: Parser[Int] = dayOfWeek(Sunday)

  def daysOfWeek: Parser[Int] = monday | tuesday | wednesday | thursday | friday | saturday | sunday

  def night: Parser[Period] = anyOf(AtNight) ^^ { _ => Night }
  def morning: Parser[Period] = anyOf(AtMorning) ^^ { _ => Morning }
  def dayAsTime: Parser[Period] = anyOf(AtDay) ^^ { _ => DayAsTime }
  def evening: Parser[Period] = anyOf(AtEvening) ^^ { _ => Evening }

  def periods: Parser[Period] = night | morning | dayAsTime | evening

}