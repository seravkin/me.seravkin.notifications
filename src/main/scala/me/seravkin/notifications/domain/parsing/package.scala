package me.seravkin.notifications.domain

import java.time.{Duration, LocalDate, LocalDateTime}

package object parsing {

  sealed trait NotificationResult {
    def shouldStop: Boolean =
      this == NotifyAndStop || this == Stop

    def shouldExecute: Boolean =
      this == NotifyAndStop || this == NotifyAndContinue

    def notIgnored: Boolean =
      this != Ignore
  }

  case class NotifyOtherUser(username: String, notificationResult: NotificationResult) extends NotificationResult {

    override def shouldStop: Boolean = notificationResult.shouldStop

    override def shouldExecute: Boolean = notificationResult.shouldExecute

    override def notIgnored: Boolean = notificationResult.notIgnored

  }
  case object NotifyAndStop extends NotificationResult
  case object NotifyAndContinue extends NotificationResult
  case object Stop extends NotificationResult
  case object Ignore extends NotificationResult

  private implicit class BooleanDateOps(boolean: Boolean) {

    private[this] def ifTrue(notificationResult: NotificationResult) =
      if(boolean) notificationResult else Ignore

    def notifyAndStopIfTrue: NotificationResult = ifTrue(NotifyAndStop)
    def notifyAndContinueIfTrue: NotificationResult = ifTrue(NotifyAndContinue)
    def stopIfTrue: NotificationResult = ifTrue(Stop)
    def ignoreIfTrue: NotificationResult = Ignore
  }

  sealed trait NotificationProgram {
    def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult
  }

  sealed trait Recurrent extends NotificationProgram

  final case class EveryDaysOfWeek(days: Set[Int], recurrent: Recurrent) extends Recurrent {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      if(days.contains(now.getDayOfWeek.ordinal()))
        recurrent.shouldNotify(startMoment, now)
      else
        Ignore
  }

  final case class InTime(hours: Int, minutes: Int) extends Recurrent {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      (now.getHour == hours &&
       now.getMinute == minutes)
        .notifyAndContinueIfTrue
  }

  final case class Before(day: Int, month: Int, year: Option[Int], recurrent: Recurrent) extends Recurrent {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      if(LocalDate.of(year.getOrElse(startMoment.getYear), month, day).isAfter(now.toLocalDate))
        Stop
      else
        recurrent.shouldNotify(startMoment, now)
  }

  final case class After(day: Int, month: Int, year: Option[Int], recurrent: Recurrent) extends Recurrent {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      if(LocalDate.of(year.getOrElse(startMoment.getYear), month, day).isBefore(now.toLocalDate))
        Ignore
      else
        recurrent.shouldNotify(startMoment, now)
  }

  sealed trait MomentInFuture extends NotificationProgram {

    def isRelativeToDate: Boolean = false

    def toExecutionTime(now: LocalDateTime): LocalDateTime

    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      now.isAfter(toExecutionTime(startMoment)).notifyAndStopIfTrue
  }

  final case class FromDuration(duration: Duration) extends MomentInFuture {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      now.plus(duration)
  }

  sealed trait Date extends MomentInFuture {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      (now.toLocalDate.isEqual(toExecutionTime(startMoment).toLocalDate) ||
       now.toLocalDate.isEqual(toExecutionTime(startMoment).toLocalDate))
          .notifyAndStopIfTrue
  }
  sealed trait Time extends MomentInFuture {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      now.toLocalTime.isAfter(toExecutionTime(startMoment).toLocalTime).notifyAndStopIfTrue
  }

  final case class InNextDayOfWeek(weekOffset: Int, dayOfWeek: Int) extends MomentInFuture with Date {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime = {

      val isAfterGivenDayOfWeek = now.getDayOfWeek.ordinal() >= dayOfWeek
      val trueWeekOffset  = if(isAfterGivenDayOfWeek) weekOffset + 1 else weekOffset

      now.minusDays(now.getDayOfWeek.ordinal()).plusDays(7 * trueWeekOffset).plusDays(dayOfWeek)
    }
  }

  final case class InDays(days: Int) extends MomentInFuture with Date {

    override def isRelativeToDate: Boolean = true

    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      now.withHour(0).withMinute(0).plusDays(days)
  }

  final case class FormattedDateWithYear(day: Int, month: Int, year: Int) extends MomentInFuture with Date {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      LocalDateTime.of(year, month, day, 0, 0)
  }

  final case class FormattedDate(day: Int, month: Int) extends MomentInFuture with Date {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      LocalDateTime.of(now.getYear, month, day, 0, 0)
  }

  final case class FormattedTime(hours: Int, minutes: Int) extends MomentInFuture with Time {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      now.withHour(hours).withMinute(minutes)
  }

  final case object InCurrentTime extends MomentInFuture with Time {
    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      now
  }

  final case class ForUser(username: String, notificationProgram: NotificationProgram) extends NotificationProgram {
    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      NotifyOtherUser(username, notificationProgram.shouldNotify(startMoment, now))

  }

  final case class FromFormattedDate(date: Date, time: Time) extends MomentInFuture {

    override def isRelativeToDate: Boolean = date.isRelativeToDate

    override def toExecutionTime(now: LocalDateTime): LocalDateTime =
      date.toExecutionTime(now)
        .withHour(time.toExecutionTime(now).getHour)
        .withMinute(time.toExecutionTime(now).getMinute)

    override def shouldNotify(startMoment: LocalDateTime, now: LocalDateTime): NotificationResult =
      (date.shouldNotify(startMoment, now), time.shouldNotify(startMoment, now)) match {
        case (NotifyAndStop, NotifyAndStop) => NotifyAndStop
        case _ => Ignore
      }
  }

}
