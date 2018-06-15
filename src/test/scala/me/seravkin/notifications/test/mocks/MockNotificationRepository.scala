package me.seravkin.notifications.test.mocks

import cats.data.State
import cats._
import cats.syntax.all._
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime, Recurrent}
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.persistance.{NotificationsRepository, Page}
import me.seravkin.notifications.test.mocks.MockBotState._

object MockNotificationRepository extends NotificationsRepository[State[MockBotState, ?]] {

  override def apply(id: Long): State[MockBotState, Option[Notification]] =
    State.get.map(_.notifications.find(n => n.id == id))

  override def apply(user: User): State[MockBotState, List[Notification]] =
    State.get.map(_.notifications.filter(n => n.userId == user.id && n.isActive))

  override def apply(user: User, skip: Int, take: Int): State[MockBotState, Page[Notification]] = for(
    count   <- apply(user).map(_.length);
    entries <- apply(user).map(_.slice(skip, skip + take))
  ) yield Page(entries, skip != 0, count > skip + entries.length)

  override def +=[T <: Notification](t: T): State[MockBotState, T] =
    State.modify[MockBotState](notifications.modify(_)(_ :+ t)) >> State.pure(t)

  private def changeIsActive(ids: Set[Long])(notification: Notification): Notification = notification match {
    case _ if !ids.contains(notification.id) => notification
    case oneTime: OneTime => oneTime.copy(isActive = false)
    case recurrent: Recurrent => recurrent.copy(isActive = false)
  }

  override def deactivate(ids: List[Long]): State[MockBotState, Unit] =
    State.modify[MockBotState](notifications.modify(_)(_.map(changeIsActive(ids.toSet))))
}