package me.seravkin.notifications.infrastructure.state

import me.seravkin.notifications.domain.User

trait ChatStateRepository[S, F[_]] {
  def get(chatId: Long): F[S]
  def set(chatId: Long, s: S): F[Unit]
}
