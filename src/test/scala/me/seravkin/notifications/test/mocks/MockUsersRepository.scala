package me.seravkin.notifications.test.mocks

import cats.data.State
import cats.syntax.all._
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.persistance.UsersRepository

final case class MockUsersRepository(user: User) extends UsersRepository[State[MockBotState, ?]] {
  override def apply(username: String): State[MockBotState, Option[User]] =
    if(user.username == username) Option(user).pure[State[MockBotState, ?]] else Option.empty[User].pure[State[MockBotState, ?]]

  override def apply(id: Long): State[MockBotState, Option[User]] =
    if(user.id == id) Option(user).pure[State[MockBotState, ?]] else Option.empty[User].pure[State[MockBotState, ?]]

  override def setChatIdIfNeeded(user: User, chatId: Long): State[MockBotState, Unit] =
    ().pure[State[MockBotState, ?]]
}