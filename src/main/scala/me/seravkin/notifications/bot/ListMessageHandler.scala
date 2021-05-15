package me.seravkin.notifications.bot

import com.bot4s.telegram.models.Message
import me.seravkin.notifications.bot.services.PageView
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.telegram.matching.{CommandWithArgs, ContainsText}

object ListMessageHandler {
  def apply[F[_]](user: PersistedUser, pageView: PageView[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText(CommandWithArgs("/list", Nil))) =>
      pageView.showPage(user, message.chat.id, 0, 3)
  }
}
