package me.seravkin.notifications.infrastructure.state

import cats.free.Free
import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, BotOp, Get, Set}

object BotIOChatStateRepository extends ChatStateRepository[ChatState, BotIO] {
  override def get(chatId: Long): BotIO[ChatState] =
    Free.liftF(Get(chatId))

  override def set(chatId: Long, s: ChatState): BotIO[Unit] =
    Free.liftF[BotOp,Unit](Set(chatId, s))
}
