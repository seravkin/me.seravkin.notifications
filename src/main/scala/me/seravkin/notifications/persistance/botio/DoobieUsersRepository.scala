package me.seravkin.notifications.persistance.botio

import cats._
import cats.data._
import cats.effect.Bracket
import doobie.implicits._
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.BotF
import me.seravkin.notifications.persistance.UsersRepository

final class DoobieUsersRepository[F[_]: Monad: Bracket[*[_], Throwable]] extends UsersRepository[BotF[F, *]] with BotIORepository[F] {
  override def apply(username: String): BotF[F, Option[PersistedUser]] = botIO {
    sql"SELECT id, chat_id, telegram_name FROM users WHERE telegram_name = $username"
      .read[PersistedUser]
      .last
  }

  override def apply(id: Long): BotF[F, Option[PersistedUser]] = botIO {
    sql"SELECT id, chat_id, telegram_name FROM users WHERE id = $id"
      .read[PersistedUser]
      .last
  }

  override def setChatIdIfNeeded(user: PersistedUser, chatId: Long): BotF[F, Unit] = user.chatId match {
    case Some(_) => ReaderT.liftF(Monad[F].unit)
    case _ => botIO {
      sql"UPDATE users SET chat_id = $chatId WHERE id = ${user.id}"
          .update
          .run
          .map(_ => ())
    }
  }
}
