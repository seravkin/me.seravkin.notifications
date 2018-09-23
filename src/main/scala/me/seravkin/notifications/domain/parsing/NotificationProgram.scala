package me.seravkin.notifications.domain.parsing

import java.time.Duration

sealed trait NotificationProgram

object NotificationProgram {
  
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

  final case class WithConfirmation(duration: Option[Duration], notificationProgram: NotificationProgram) extends MomentInFuture


}

