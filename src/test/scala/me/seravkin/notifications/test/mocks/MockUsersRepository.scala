package me.seravkin.notifications.test.mocks

import cats.Monad
import cats.data.StateT
import cats.syntax.all._
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.persistance.UsersRepository

final class MockUsersRepository[F[_]: Monad](user: PersistedUser) extends UsersRepository[StateT[F, MockBotState, *]] {
  override def apply(username: String): StateT[F, MockBotState, Option[PersistedUser]] =
    if(user.username == username)
      Option(user).pure[StateT[F, MockBotState, *]]
    else
      Option.empty[PersistedUser].pure[StateT[F, MockBotState, *]]

  override def apply(id: Long): StateT[F, MockBotState, Option[PersistedUser]] =
    if(user.id == id)
      Option(user).pure[StateT[F, MockBotState, *]]
    else
      Option.empty[PersistedUser].pure[StateT[F, MockBotState, *]]

  override def setChatIdIfNeeded(user: PersistedUser, chatId: Long): StateT[F, MockBotState, Unit] =
    ().pure[StateT[F, MockBotState, *]]
}