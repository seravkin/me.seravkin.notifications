package me.seravkin.notifications.infrastructure.time
import java.time.LocalDateTime

object ActualSystemDateTime extends SystemDateTime {
  override def now: LocalDateTime = LocalDateTime.now()
}
