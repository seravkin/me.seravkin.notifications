package me.seravkin.notifications.bot

import com.bot4s.telegram.models.Message
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.telegram.matching.ContainsText

object VersionHandler {
  def apply[F[_]](sender: Sender[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText("/version")) =>
      sender.tell(message.chat.id, me.seravkin.notifications.BuildInfo.version)
  }
}
