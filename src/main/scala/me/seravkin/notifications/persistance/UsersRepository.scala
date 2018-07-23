package me.seravkin.notifications.persistance

import me.seravkin.notifications.domain.PersistedUser

trait UsersRepository[F[_]] {
  def apply(id: Long): F[Option[PersistedUser]]
  def apply(username: String): F[Option[PersistedUser]]

  def setChatIdIfNeeded(user: PersistedUser, chatId: Long): F[Unit]
}
