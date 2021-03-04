package me.seravkin.notifications.infrastructure.time
import cats.effect.Sync

import java.time.LocalDateTime

final class ActualSystemDateTime[F[_]: Sync] extends SystemDateTime[F] {
  override def now: F[LocalDateTime] = Sync[F].delay(LocalDateTime.now())
}
