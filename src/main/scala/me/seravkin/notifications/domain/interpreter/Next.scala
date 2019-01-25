package me.seravkin.notifications.domain.interpreter

import java.time.LocalDateTime

trait Next[T] {
  def next(now: LocalDateTime): Option[T]
}
