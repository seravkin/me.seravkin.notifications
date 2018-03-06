package me.seravkin.notifications.infrastructure.state

import me.seravkin.notifications.domain.User

trait ChatStateRepository[S, F[_]] {
  def get(): F[S]
  def set(s: S): F[Unit]
}
