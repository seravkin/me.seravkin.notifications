package me.seravkin.notifications.persistance

import me.seravkin.notifications.domain.User

trait UsersRepository[F[_]] {
  def apply(id: Long): F[Option[User]]
  def apply(username: String): F[Option[User]]

  def setChatIdIfNeeded(user: User, chatId: Long): F[Unit]
}
