package me.seravkin.notifications.domain.interpreter

import java.time.{Duration, LocalDateTime}

sealed trait Dates {
  def notificationDate: LocalDateTime
}

object Dates {

  final case class OneDate(localDateTime: LocalDateTime) extends Dates with Next[OneDate] {
    override def next(now: LocalDateTime): Option[OneDate] = None

    override def notificationDate: LocalDateTime = localDateTime
  }

  final case class Confirmation(localDateTime: LocalDateTime, period: Duration) extends Dates with Next[Confirmation] {
    def next(now: LocalDateTime): Option[Confirmation] =
      Some(Confirmation(localDateTime.plus(period), period))

    override def notificationDate: LocalDateTime =
      localDateTime
  }

  sealed trait RecurrencyType {
    def startDay(now: LocalDateTime): Int
    def startDayOnNextPeriod(now: LocalDateTime, current: Int, days: Set[Int]): LocalDateTime
  }

  object RecurrencyType {
    case object Week extends RecurrencyType {
      override def startDay(now: LocalDateTime): Int =
        now.getDayOfWeek.getValue - 1

      override def startDayOnNextPeriod(now: LocalDateTime, current: Int, days: Set[Int]): LocalDateTime =
        now.plusDays(7 - current + days.min)
    }
    case object Month extends RecurrencyType {
      override def startDay(now: LocalDateTime): Int =
        now.getDayOfMonth - 1

      override def startDayOnNextPeriod(now: LocalDateTime, current: Int, days: Set[Int]): LocalDateTime =
        now.plusMonths(1).withDayOfMonth(days.min + 1)
    }
  }

  final case class Periodic(localDateTime: LocalDateTime,
                            hour: Int, minutes: Int, days: Set[Int], recurrencyType: RecurrencyType,
                            start: Option[LocalDateTime], end: Option[LocalDateTime]) extends Dates with Next[Periodic] {
    override def next(now: LocalDateTime): Option[Periodic] =
      Periodic(hour, minutes, days, recurrencyType, start, end)(now)

    override def notificationDate: LocalDateTime =
      localDateTime
  }

  object Periodic {

    private def dateWithoutConstraints(now: LocalDateTime, hour: Int, minutes: Int,
                                       days: Set[Int], recurrencyType: RecurrencyType): LocalDateTime = {
      val current = recurrencyType.startDay(now)

      val possibleDates =
        days
          .filter(_ >= current)
          .map(d => now.plusDays(d - current).withHour(hour).withMinute(minutes))

      val datesAfter = possibleDates
        .filter(_.isAfter(now))

      if (datesAfter.isEmpty) {
        recurrencyType.startDayOnNextPeriod(now, current, days).withHour(hour).withMinute(minutes)
      } else {
        datesAfter.minBy(_.getDayOfWeek.getValue)
      }
    }

    private def nextDate(now: LocalDateTime, hour: Int, minutes: Int,
                         days: Set[Int],
                         recurrencyType: RecurrencyType,
                         start: Option[LocalDateTime],
                         end: Option[LocalDateTime]): Option[LocalDateTime] = {
      if (start.exists(_.isAfter(now))) {
        Some(dateWithoutConstraints(start.get, hour, minutes, days, recurrencyType))
      } else if (end.exists(dateWithoutConstraints(now, hour, minutes, days, recurrencyType).isAfter(_))) {
        None
      } else {
        Some(dateWithoutConstraints(now, hour, minutes, days, recurrencyType))
      }
    }

    def apply(hour: Int, minutes: Int, days: Set[Int], reccurencyType: RecurrencyType,
              start: Option[LocalDateTime], end: Option[LocalDateTime])(now: LocalDateTime): Option[Periodic] =
      nextDate(now, hour, minutes, days, reccurencyType, start, end).map(new Periodic(_, hour, minutes, days,
                                                                                         reccurencyType, start, end))
  }


}
