package me.seravkin.notifications.test.mocks

import cats.data.State
import cats._
import cats.syntax.all._
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.test.mocks.MockBotState._

object MockSender extends Sender[State[MockBotState, ?]] {
  override def send(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): State[MockBotState, Int] =
    State.modify[MockBotState](messages.modify(_)(_ :+ MockMessage(User(1, Some(chatId), ""), text, buttons = buttonWithCommand))) >>
    State.get[MockBotState].map(_.sentMessages.length)
}
