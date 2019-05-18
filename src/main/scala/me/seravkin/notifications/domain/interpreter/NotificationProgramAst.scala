package me.seravkin.notifications.domain.interpreter

import java.time.Duration

import me.seravkin.notifications.domain.parsing.NotificationProgram._
import me.seravkin.notifications.domain.parsing._
import me.seravkin.notifications.domain.ast._

object NotificationProgramAst extends DateAst[NotificationProgram]
  with RecurrentAst[NotificationProgram] with DurationAst[NotificationProgram]
  with RelativeAst[NotificationProgram] with TimeAst[NotificationProgram]
  with UserAst[NotificationProgram] with ConfirmationAst[NotificationProgram]
  with DateAndTimeAst[NotificationProgram, NotificationProgram, NotificationProgram] {

  override def dayOfWeek(weekOffset: Int, dayOfWeek: Int): NotificationProgram =
    InNextDayOfWeek(weekOffset, dayOfWeek)

  override def inDays(days: Int): NotificationProgram =
    InDays(days)

  override def duration(duration: Duration): NotificationProgram =
    FromDuration(duration)

  override def date(day: Int, month: Int, year: Int): NotificationProgram =
    FormattedDateWithYear(day, month, year)

  override def date(day: Int, month: Int): NotificationProgram =
    FormattedDate(day, month)

  override def time(hours: Int, minutes: Int): NotificationProgram =
    FormattedTime(hours, minutes)

  override def inCurrentTime: NotificationProgram =
    InCurrentTime

  override def dateAndTime(date: NotificationProgram, time: NotificationProgram): NotificationProgram =
    FromFormattedDate(date, time)

  override def fuzzyTime(period: Period): NotificationProgram =
    AtFuzzyTime(period)

  override def forUser(username: String, t: NotificationProgram): NotificationProgram =
    ForUser(username, t)

  override def everyDayOfWeek(days: Set[Int], t: NotificationProgram): NotificationProgram =
    EveryDaysOfWeek(days, t)

  override def before(day: Int, month: Int, year: Option[Int], recurrent: NotificationProgram): NotificationProgram =
    Before(day, month, year, recurrent)

  override def after(day: Int, month: Int, year: Option[Int], recurrent: NotificationProgram): NotificationProgram =
    After(day, month, year, recurrent)

  override def inTime(hours: Int, minutes: Int): NotificationProgram =
    InTime(hours, minutes)

  override def confirmation(duration: Option[Duration], t: NotificationProgram): NotificationProgram =
    WithConfirmation(duration, t)

  override def everyDayOfMonth(days: Set[Int], t: NotificationProgram): NotificationProgram =
    EveryDaysOfMonth(days, t)
}
