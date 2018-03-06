package me.seravkin.notifications.infrastructure.messages

import cats.free.Free
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, Send}

object BotIOSender extends Sender[BotIO] {
  override def send(chatId: Long, text: String, buttonWithCommand: List[Button]): BotIO[Unit] =
    Free.liftF(Send(chatId, text, buttonWithCommand))
}
