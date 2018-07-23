package me.seravkin.notifications.infrastructure.messages

import cats.free.Free
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, Send}

object BotIOSender extends Sender[BotIO] {
  override def ask(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): BotIO[Int] =
    Free.liftF(Send(chatId, text, buttonWithCommand, idToEdit))
}
