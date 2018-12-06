package me.seravkin.notifications.bot

import cats.Monad
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.tg.adapter.matching.ContainsText

object VersionHandler {
  def apply[F[_]: Monad](sender: Sender[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText("/version")) =>
      sender.tell(message.chat.id, "0.3.2")
  }
}
