package me.seravkin.notifications.domain

import java.time.{Duration, LocalDateTime}

import cats.data.{EitherT, ReaderT}
import me.seravkin.notifications.domain.Notifications.Notification

package object interpreter {

  sealed trait Dates {
    def notificationDate: LocalDateTime
  }

  sealed trait Next[T] {
    def next(now: LocalDateTime): T
  }

  final case class OneDate(localDateTime: LocalDateTime) extends Dates with Next[OneDate] {
    override def next(now: LocalDateTime): this.type = this
    override def notificationDate: LocalDateTime = localDateTime
  }
  final case class Confirmation(localDateTime: LocalDateTime, period: Duration) extends Dates with Next[Confirmation] {
    def next(now: LocalDateTime): Confirmation =
      Confirmation(localDateTime.plus(period), period)

    override def notificationDate: LocalDateTime =
      localDateTime
  }
  final case class Periodic(localDateTime: LocalDateTime,
                            hour: Int, minutes: Int, days: Set[Int],
                            start: Option[LocalDateTime], end: Option[LocalDateTime]) extends Dates with Next[Periodic] {
    override def next(now: LocalDateTime): Periodic =
      Periodic(hour, minutes, days, start, end)(now)

    override def notificationDate: LocalDateTime =
      localDateTime
  }

  object Periodic {

    private def nextDate(now: LocalDateTime, hour: Int, minutes: Int, days: Set[Int]): LocalDateTime = {
      val current = now.getDayOfWeek.getValue
      if(days.forall(current > _)) {
        now.plusDays(7 - current + days.min).withHour(hour).withMinute(minutes)
      } else {
        now.plusDays(days.filter(_ >= current).min - current).withHour(hour).withMinute(minutes)
      }
    }

    def apply(hour: Int, minutes: Int, days: Set[Int], start: Option[LocalDateTime], end: Option[LocalDateTime])(now: LocalDateTime): Periodic =
      new Periodic(nextDate(now, hour, minutes, days), hour, minutes, days, start, end)
  }

  type DatesFactory[F[_]] = ReaderT[F, LocalDateTime, Either[String, Dates]]

}
