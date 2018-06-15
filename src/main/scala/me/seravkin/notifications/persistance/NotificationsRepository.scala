package me.seravkin.notifications.persistance

import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.User

trait NotificationsRepository[F[_]] {

  def apply(id: Long): F[Option[Notification]]
  def apply(user: User): F[List[Notification]]
  def apply(user: User, skip: Int, take: Int): F[Page[Notification]]

  def +=[T <: Notification](t: T): F[T]

  def deactivate(ids: List[Long]): F[Unit]
}
