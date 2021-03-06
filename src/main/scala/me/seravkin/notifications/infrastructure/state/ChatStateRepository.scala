package me.seravkin.notifications.infrastructure.state

trait ChatStateRepository[S, F[_]] {
  def get(chatId: Long): F[S]
  def set(chatId: Long, s: S): F[Unit]
}
