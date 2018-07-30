package me.seravkin.notifications.domain

import java.time.{Duration, LocalDate, LocalDateTime}

import me.seravkin.notifications.domain.internationalization.Words.DayOfWeek

import scala.util.Random

package object parsing {

  trait RecurrentAst[T] {
    def everyDayOfWeek(days: Set[Int], t: T): T
    def before(day: Int, month: Int, year: Option[Int], recurrent: T): T
    def after(day: Int, month: Int, year: Option[Int], recurrent: T): T
    def inTime(hours: Int, minutes: Int): T
  }

  trait MomentInFutureAst[T] {

    def dayOfWeek(weekOffset: Int, dayOfWeek: Int): T

    def inDays(days: Int): T

    def duration(duration: Duration): T

    def date(day: Int, month: Int, year: Int): T
    def date(day: Int, month: Int): T
    
    def time(hours: Int, minutes: Int): T
    def inCurrentTime: T

    def dateAndTime(date: T, time: T): T

    def fuzzyTime(period: Period): T

    def forUser(username: String, t: T): T

  }

  sealed trait NotificationProgram

  final case class ForUser(username: String, notificationProgram: NotificationProgram) extends NotificationProgram

  sealed trait Recurrent extends NotificationProgram

  final case class EveryDaysOfWeek(days: Set[Int], recurrent: NotificationProgram) extends Recurrent
  final case class InTime(hours: Int, minutes: Int) extends Recurrent
  final case class Before(day: Int, month: Int, year: Option[Int], recurrent: NotificationProgram) extends Recurrent
  final case class After(day: Int, month: Int, year: Option[Int], recurrent: NotificationProgram) extends Recurrent

  sealed trait MomentInFuture extends NotificationProgram

  final case class FromDuration(duration: Duration) extends MomentInFuture
  final case class FromFormattedDate(date: NotificationProgram, time: NotificationProgram) extends MomentInFuture

  sealed trait Date extends MomentInFuture
  sealed trait Time extends MomentInFuture

  final case class InNextDayOfWeek(weekOffset: Int, dayOfWeek: Int) extends MomentInFuture with Date
  final case class InDays(days: Int) extends MomentInFuture with Date
  final case class FormattedDateWithYear(day: Int, month: Int, year: Int) extends MomentInFuture with Date
  final case class FormattedDate(day: Int, month: Int) extends MomentInFuture with Date

  final case class FormattedTime(hours: Int, minutes: Int) extends MomentInFuture with Time
  final case class AtFuzzyTime(period: Period) extends MomentInFuture with Time
  final case object InCurrentTime extends MomentInFuture with Time

  sealed trait Period { def period: (Int, Int); }
  final case object Night extends Period { override def period: (Int, Int) = 0 -> 8 }
  final case object Morning extends Period { override def period: (Int, Int) = 8 -> 12 }
  final case object DayAsTime extends Period { override def period: (Int, Int) = 12 -> 19 }
  final case object Evening extends Period { override def period: (Int, Int) = 19 -> 23 }

}
