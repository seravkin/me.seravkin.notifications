package me.seravkin.notifications.infrastructure.time

import java.time.LocalDateTime

trait SystemDateTime {
  def now: LocalDateTime
}
