package me.seravkin.notifications.test

import java.time.LocalDateTime

import cats.data._
import cats._
import cats.syntax.all._
import me.seravkin.notifications.bot.NotificationBot.{ChatState, Nop}
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime, Recurrent}
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.infrastructure.messages.{Button, Message, Sender}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, UsersRepository}
import shapeless._

trait InfrastructureMocks {
  final case class TestMessage(user: User, text: String, data: Option[String] = None, buttons: List[Button] = Nil)

  final case class MockDateTime(dateTime: LocalDateTime) extends SystemDateTime {
    override def now: LocalDateTime = dateTime
  }

  implicit val messageInstance: Message[TestMessage] = new Message[TestMessage] {

    override def username(msg: TestMessage): Option[String] = Some(msg.user.username)

    override def text(msg: TestMessage): Option[String] = Some(msg.text)

    override def chatId(msg: TestMessage): Long = msg.user.chatId.get

    override def data(msg: TestMessage): Option[String] = msg.data

    override def isPrivate(msg: TestMessage): Boolean = true
  }

  final case class BotState(users: List[User] = Nil,
                            notifications: List[Notification] = Nil,
                            sentMessages: List[TestMessage] = Nil,
                            chatState: ChatState = Nop)

  protected val messages = lens[BotState] >> 'sentMessages
  protected val state = lens[BotState] >> 'chatState
  protected val notifications = lens[BotState] >> 'notifications

  object TestSender extends Sender[State[BotState, ?]] {
    override def send(chatId: Long, text: String, buttonWithCommand: List[Button]): State[BotState, Unit] =
      State.modify(messages.modify(_)(_ :+ TestMessage(User(1, Some(chatId), ""), text, buttons = buttonWithCommand)))
  }

  case class TestUsersRepository(user: User) extends UsersRepository[State[BotState, ?]] {
    override def apply(username: String): State[BotState, Option[User]] =
      if(user.username == username) Option(user).pure[State[BotState, ?]] else Option.empty[User].pure[State[BotState, ?]]

    override def apply(id: Long): State[BotState, Option[User]] =
      if(user.id == id) Option(user).pure[State[BotState, ?]] else Option.empty[User].pure[State[BotState, ?]]

    override def setChatIdIfNeeded(user: User, chatId: Long): State[BotState, Unit] =
      ().pure[State[BotState, ?]]
  }

  object TestNotificationRepository extends NotificationsRepository[State[BotState, ?]] {

    override def apply(id: Long): State[BotState, Option[Notification]] =
      State.get.map(_.notifications.find(n => n.id == id))

    override def apply(user: User): State[BotState, List[Notification]] =
      State.get.map(_.notifications.filter(n => n.userId == user.id && n.isActive))

    override def +=[T <: Notification](t: T): State[BotState, T] =
      State.modify[BotState](notifications.modify(_)(_ :+ t)) >> State.pure(t)

    private def changeIsActive(ids: Set[Long])(notification: Notification): Notification = notification match {
      case _ if !ids.contains(notification.id) => notification
      case oneTime: OneTime => oneTime.copy(isActive = false)
      case recurrent: Recurrent => recurrent.copy(isActive = false)
    }

    override def deactivate(ids: List[Long]): State[BotState, Unit] =
      State.modify[BotState](notifications.modify(_)(_.map(changeIsActive(ids.toSet))))
  }

  object TestChatStateRepository extends ChatStateRepository[ChatState, State[BotState, ?]] {
    override def get(): State[BotState, ChatState] =
      State.get.map(_.chatState)

    override def set(s: ChatState): State[BotState, Unit] =
      State.modify(state.set(_)(s))
  }
}
