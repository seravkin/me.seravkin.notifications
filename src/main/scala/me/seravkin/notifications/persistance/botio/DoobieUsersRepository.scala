package me.seravkin.notifications.persistance.botio

import cats.free.Free
import doobie._
import doobie.implicits._
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, BotOp}
import me.seravkin.notifications.persistance.UsersRepository

object DoobieUsersRepository extends UsersRepository[BotIO] with BotIORepository {
  override def apply(username: String): BotIO[Option[User]] = botIO {
    sql"SELECT id, chat_id, telegram_name FROM users WHERE telegram_name = $username"
      .read[User]
      .last
  }

  override def apply(id: Long): BotIO[Option[User]] = botIO {
    sql"SELECT id, chat_id, telegram_name FROM users WHERE id = $id"
      .read[User]
      .last
  }

  override def setChatIdIfNeeded(user: User, chatId: Long): BotIO[Unit] = user.chatId match {
    case Some(_) => Free.pure[BotOp, Unit]()
    case _ => botIO {
      sql"UPDATE users SET chat_id = $chatId WHERE id = ${user.id}"
          .update
          .run
          .map(_ => ())
    }
  }
}
