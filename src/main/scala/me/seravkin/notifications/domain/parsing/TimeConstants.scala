package me.seravkin.notifications.domain.parsing

import java.time.Duration

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.parsing.Period._

final class TimeConstants(intr: InternalizationParsers) {

  private[this] def word(word: Word, f: Long => Duration): Parser[Int => Duration] =
    intr.anyOf(word).map { _ => i: Int => f(i) }

  private[this] def dayOfWeek[T <: Word with DayOfWeek](t: T): Parser[Int] =
    intr.anyOf(t).map { _ => t.toInt }

  private[this] def month[T <: Word with Month](t: T): Parser[Month] =
    intr.anyOf(t) >| t

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

  def night: Parser[Period] = intr.anyOf(AtNight) >| Night
  def morning: Parser[Period] = intr.anyOf(AtMorning) >| Morning
  def dayAsTime: Parser[Period] = intr.anyOf(AtDay) >| DayAsTime
  def evening: Parser[Period] = intr.anyOf(AtEvening) >| Evening

  def periods: Parser[Period] = night | morning | dayAsTime | evening

}