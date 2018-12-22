package me.seravkin.notifications.domain.ast

import java.time.Duration

trait DurationAst[T] {
  def duration(duration: Duration): T
}
