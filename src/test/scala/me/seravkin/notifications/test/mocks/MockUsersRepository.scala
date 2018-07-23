package me.seravkin.notifications.test.mocks

import cats.data.State
import cats.syntax.all._
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.persistance.UsersRepository

final case class MockUsersRepository(user: PersistedUser) extends UsersRepository[State[MockBotState, ?]] {
  override def apply(username: String): State[MockBotState, Option[PersistedUser]] =
    if(user.username == username) Option(user).pure[State[MockBotState, ?]] else Option.empty[PersistedUser].pure[State[MockBotState, ?]]

  override def apply(id: Long): State[MockBotState, Option[PersistedUser]] =
    if(user.id == id) Option(user).pure[State[MockBotState, ?]] else Option.empty[PersistedUser].pure[State[MockBotState, ?]]

  override def setChatIdIfNeeded(user: PersistedUser, chatId: Long): State[MockBotState, Unit] =
    ().pure[State[MockBotState, ?]]
}