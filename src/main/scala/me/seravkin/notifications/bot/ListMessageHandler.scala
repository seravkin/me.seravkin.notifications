package me.seravkin.notifications.bot

import cats._
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.bot.services.PageView
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.tg.adapter.matching.{CommandWithArgs, ContainsText}

object ListMessageHandler {
  def apply[F[_]: Monad](user: PersistedUser, pageView: PageView[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText(CommandWithArgs("/list", Nil))) =>
      pageView.showPage(user, message.chat.id, 0, 3)
  }
}
