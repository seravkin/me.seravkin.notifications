package me.seravkin.notifications.test.mocks

import cats.data.{State, StateT}
import cats._
import cats.syntax.all._
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.persistance.{NotificationsRepository, Page}
import me.seravkin.notifications.test.mocks.MockBotState._

final class MockNotificationRepository[F[_]: Monad] extends NotificationsRepository[StateT[F, MockBotState, ?]] {

  override def apply(id: Long): StateT[F, MockBotState, Option[Notification]] =
    StateT.get[F, MockBotState].map(_.notifications.find(n => n.id == id))

  override def apply(user: PersistedUser): StateT[F, MockBotState, List[Notification]] =
    StateT.get[F, MockBotState].map(_.notifications.filter(n => n.userId == user.id && n.isActive))

  override def apply(user: PersistedUser, skip: Int, take: Int): StateT[F, MockBotState, Page[Notification]] = for(
    count   <- apply(user).map(_.length);
    entries <- apply(user).map(_.slice(skip, skip + take))
  ) yield Page(entries, skip != 0, count > skip + entries.length)

  override def +=[T <: Notification](t: T): StateT[F, MockBotState, T] =
    StateT.modify[F, MockBotState](st => st.copy(notifications = st.notifications :+ t)) >> StateT.pure[F, MockBotState, T](t)

  private def changeIsActive(ids: Set[Long])(notification: Notification): Notification = notification match {
    case _ if !ids.contains(notification.id) => notification
    case _ => notification.copy(isActive = false)
  }

  private def changeTextById(id: Long, text: String)(notification: Notification): Notification = notification match {
    case _ if id != notification.id => notification
    case _ => notification.copy(text = text)
  }

  override def deactivate(ids: List[Long]): StateT[F, MockBotState, Unit] =
    StateT.modify[F, MockBotState](notifications.modify(_)(_.map(changeIsActive(ids.toSet))))

  override def update(id: Long, text: String): StateT[F, MockBotState, Unit] =
    StateT.modify[F, MockBotState](notifications.modify(_)(_.map(changeTextById(id, text))))
}