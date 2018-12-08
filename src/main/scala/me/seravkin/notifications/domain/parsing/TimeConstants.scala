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

  private[this] def month[T <: Word with Month](t: T): Parser[Month] =
    anyOf(t) ^^ { _ => t }

  def second: Parser[Int => Duration] = word(Second, Duration.ofSeconds)
  def minute: Parser[Int => Duration] = word(Minute, Duration.ofMinutes)
  def hour: Parser[Int => Duration] = word(Hour, Duration.ofHours)
  def day: Parser[Int => Duration] = word(Day, Duration.ofDays)
  def week: Parser[Int => Duration] = word(Week, i => Duration.ofDays(i * 7))

  def monday: Parser[Int] = dayOfWeek(DayOfWeek.Monday)
  def tuesday: Parser[Int] = dayOfWeek(DayOfWeek.Tuesday)
  def wednesday: Parser[Int] = dayOfWeek(DayOfWeek.Wednesday)
  def thursday: Parser[Int] = dayOfWeek(DayOfWeek.Thursday)
  def friday: Parser[Int] = dayOfWeek(DayOfWeek.Friday)
  def saturday: Parser[Int] = dayOfWeek(DayOfWeek.Saturday)
  def sunday: Parser[Int] = dayOfWeek(DayOfWeek.Sunday)

  def months: Parser[Month] =
    month(Month.January)  |
    month(Month.February) |
    month(Month.March)    |
    month(Month.April)    |
    month(Month.May)      |
    month(Month.June)     |
    month(Month.July)     |
    month(Month.August)   |
    month(Month.September)|
    month(Month.October)  |
    month(Month.November) |
    month(Month.December)

  def daysOfWeek: Parser[Int] = monday | tuesday | wednesday | thursday | friday | saturday | sunday

  def night: Parser[Period] = anyOf(AtNight) ^^ { _ => Night }
  def morning: Parser[Period] = anyOf(AtMorning) ^^ { _ => Morning }
  def dayAsTime: Parser[Period] = anyOf(AtDay) ^^ { _ => DayAsTime }
  def evening: Parser[Period] = anyOf(AtEvening) ^^ { _ => Evening }

  def periods: Parser[Period] = night | morning | dayAsTime | evening

}