package me.seravkin.notifications.test.mocks

import me.seravkin.notifications.domain.User
import me.seravkin.notifications.infrastructure.messages.{Button, Message}

final case class MockMessage(id: Int, user: User, text: String, data: Option[String] = None, buttons: List[Button] = Nil)

object MockMessage {
  implicit val messageInstance: Message[MockMessage] = new Message[MockMessage] {

    override def username(msg: MockMessage): Option[String] = Some(msg.user.username)

    override def text(msg: MockMessage): Option[String] = Some(msg.text)

    override def chatId(msg: MockMessage): Long = msg.user.chatId.get

    override def data(msg: MockMessage): Option[String] = msg.data

    override def isPrivate(msg: MockMessage): Boolean = true
  }
}