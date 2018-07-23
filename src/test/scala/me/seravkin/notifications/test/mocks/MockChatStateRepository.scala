package me.seravkin.notifications.test.mocks

import cats.data.State
import cats.syntax.all._
import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.test.mocks.MockBotState._

object MockChatStateRepository extends ChatStateRepository[ChatState, State[MockBotState, ?]] {
  override def get(chatId: Long): State[MockBotState, ChatState] =
    State.get.map(_.chatState)

  override def set(chatId: Long, s: ChatState): State[MockBotState, Unit] =
    State.modify(state.set(_)(s))
}
