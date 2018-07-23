package me.seravkin.notifications.domain.algebra

import cats.free.Free
import doobie.free.connection.ConnectionIO
import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.infrastructure.messages.Button

object BotAlgebra {
  sealed trait BotOp[A]

  final case class Send(chatId: Long, text: String, buttons: List[Button] = Nil, idToEdit: Option[Int] = None) extends BotOp[Int]
  final case class Set(chatId: Long, state: ChatState) extends BotOp[Unit]
  final case class Get(chatId: Long) extends BotOp[ChatState]
  final case class DatabaseAction[T](connectionIO: ConnectionIO[T]) extends BotOp[T]

  type BotIO[A] = Free[BotOp, A]
}
