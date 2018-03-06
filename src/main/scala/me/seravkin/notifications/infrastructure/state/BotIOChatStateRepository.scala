package me.seravkin.notifications.infrastructure.state

import cats.free.Free
import me.seravkin.notifications.bot.NotificationBot.ChatState
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, BotOp, Get, Set}

object BotIOChatStateRepository extends ChatStateRepository[ChatState, BotIO] {
  override def get(): BotIO[ChatState] =
    Free.liftF(Get())

  override def set(s: ChatState): BotIO[Unit] =
    Free.liftF[BotOp,Unit](Set(s))
}
