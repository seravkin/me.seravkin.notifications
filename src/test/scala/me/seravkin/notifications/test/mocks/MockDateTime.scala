package me.seravkin.notifications.test.mocks

import cats.Applicative

import java.time.LocalDateTime
import me.seravkin.notifications.infrastructure.time.SystemDateTime

final case class MockDateTime[F[_]: Applicative](dateTime: LocalDateTime) extends SystemDateTime[F] {
  override def now: F[LocalDateTime] = Applicative[F].pure(dateTime)
}
