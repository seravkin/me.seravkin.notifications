package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import com.bot4s.telegram.models.Message
import me.seravkin.notifications.bot.services.NotificationChatService
import me.seravkin.notifications.infrastructure.messages.Message.{CommandWithArgs, IsLong}
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.telegram.matching.ContainsText
import me.seravkin.notifications.persistance.NotificationsRepository

object OldEditHandler {
  def apply[F[_]:Monad](notificationsRepository: NotificationsRepository[F],
                        notificationChatService: NotificationChatService[F],
                        sender: Sender[F]): BotHandler[Message, F] = {

    case HasMessage(message@ContainsText(CommandWithArgs("/delete", IsLong(id) :: Nil))) =>
      notificationsRepository.deactivate(id :: Nil) >>
      sender.tell(message.chat.id, "Напоминание удалено")

    case HasMessage(message@ContainsText(CommandWithArgs("/change", IsLong(id) :: Nil))) =>
      notificationChatService.changeNotificationDate(message.chat.id, id)

  }
}
