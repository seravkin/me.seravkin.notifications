package me.seravkin.notifications.test.mocks

import cats.Applicative
import cats.data.{State, StateT}
import cats.effect.IO
import cats.syntax.all._
import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.test.MockBotF
import me.seravkin.notifications.test.mocks.MockBotState._

final class MockChatStateRepository[F[_]: Applicative] extends ChatStateRepository[ChatState, StateT[F, MockBotState, ?]] {
  override def get(chatId: Long): StateT[F, MockBotState, ChatState] =
    StateT.get[F, MockBotState].map(_.chatState)

  override def set(chatId: Long, s: ChatState): StateT[F, MockBotState, Unit] =
    StateT.modify[F, MockBotState](state.set(_)(s))
}
