package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import com.bot4s.telegram.models.Message
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.persistance.NotificationsRepository
import me.seravkin.tg.adapter.matching._

object OldShowHandler {

  def apply[F[_]: Monad](user: PersistedUser, notificationsRepository: NotificationsRepository[F], sender: Sender[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText("/show")) =>
      for (notifications <- notificationsRepository(user);
           _             <- sender.ask(message.chat.id, notifications.show))
        yield ()

  }
}
