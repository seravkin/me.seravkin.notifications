package me.seravkin.notifications.domain.parsing

import java.time.Duration

import scala.util.parsing.combinator.RegexParsers

trait TimeConstants { this: RegexParsers =>

  def second: Parser[Int => Duration] = ("секунда" | "секунды" |  "секунд" | "сек" | "c" | "seconds" | "second" | "sec" | "s") ^^ { _ => i : Int => Duration.ofSeconds(i) }
  def minute: Parser[Int => Duration] = ("минуты" | "минута" | "минут" | "мин" | "м" | "minutes" | "minute" | "min" | "m") ^^ { _ => i : Int => Duration.ofMinutes(i) }
  def hour: Parser[Int => Duration] = ("часов" | "часа" | "час" | "ч" | "hours" | "hour" | "h") ^^ { _ => i : Int => Duration.ofHours(i) }
  def day: Parser[Int => Duration] = ("дней" | "день" | "дня" | "д"| "days" | "day" | "d") ^^ { _ => i : Int => Duration.ofDays(i) }
  def week: Parser[Int => Duration] = ("неделю" | "недели" | "неделя" | "недель" | "нед" | "week" | "weeks") ^^ { _ => i : Int => Duration.ofDays(7 * i) }

  def monday: Parser[Int] = ("понедельник" | "пн" | "monday" | "mon") ^^ { _ => 0 }
  def tuesday: Parser[Int] = ("вторник" | "вт" | "tuesday" | "tue") ^^ { _ => 1 }
  def wednesday: Parser[Int] = ("среда" | "среду" | "ср" | "wednesday" | "wed") ^^ { _ => 2 }
  def thursday: Parser[Int] = ("четверг" | "чт" | "thursday" | "thu") ^^ { _ => 3 }
  def friday: Parser[Int] = ("пятница" | "пятницу" | "пт" | "friday" | "fri") ^^ { _ => 4 }
  def saturday: Parser[Int] = ("суббота" | "субботу" | "сб" | "saturday" | "sat") ^^ { _ => 5 }
  def sunday: Parser[Int] = ("воскресенье" | "вс" | "sunday" | "sun") ^^ { _ => 6 }

  def daysOfWeek: Parser[Int] = monday | tuesday | wednesday | thursday | friday | saturday | sunday

}