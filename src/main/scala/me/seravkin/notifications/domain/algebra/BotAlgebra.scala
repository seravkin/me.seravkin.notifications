package me.seravkin.notifications.domain.algebra

import cats.free.Free
import doobie.free.connection.ConnectionIO
import info.mukel.telegrambot4s.models.ChatId
import me.seravkin.notifications.bot.NotificationBot.ChatState
import me.seravkin.notifications.infrastructure.messages.Button

object BotAlgebra {
  sealed trait BotOp[A]

  final case class Send(chatId: Long, text: String, buttons: List[Button] = Nil, idToEdit: Option[Int] = None) extends BotOp[Int]
  final case class Set[S](state: S) extends BotOp[Unit]
  final case class Get[S]() extends BotOp[S]
  final case class DatabaseAction[T](connectionIO: ConnectionIO[T]) extends BotOp[T]

  type BotIO[A] = Free[BotOp, A]
}
