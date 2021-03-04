package me.seravkin.notifications.infrastructure.time

import java.time.LocalDateTime

trait SystemDateTime[F[_]] {
  def now: F[LocalDateTime]
}
