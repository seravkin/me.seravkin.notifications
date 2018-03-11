package me.seravkin.notifications.test.mocks

import cats.data.State
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.test.mocks.MockBotState._

object MockSender extends Sender[State[MockBotState, ?]] {
  override def send(chatId: Long, text: String, buttonWithCommand: List[Button]): State[MockBotState, Unit] =
    State.modify(messages.modify(_)(_ :+ MockMessage(User(1, Some(chatId), ""), text, buttons = buttonWithCommand)))
}
