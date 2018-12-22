package me.seravkin.notifications.domain.ast

import java.time.Duration

trait ConfirmationAst[T] {
  def confirmation(duration: Option[Duration], t: T): T
}
