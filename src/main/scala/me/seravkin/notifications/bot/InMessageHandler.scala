package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import com.bot4s.telegram.models.Message
import me.seravkin.notifications.bot.ChatState._
import me.seravkin.notifications.bot.services.NotificationChatService
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.Message.{CommandWithQuotedArgs, TailAsText}
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.telegram.matching.ContainsText
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.NotificationsRepository

object InMessageHandler {

  def apply[F[_]: Monad](user: PersistedUser,
                         systemDateTime: SystemDateTime[F],
                         chatStateRepository: ChatStateRepository[ChatState, F],
                         notificationChatService: NotificationChatService[F],
                         notificationsRepository: NotificationsRepository[F],
                         sender: Sender[F]): BotHandler[Message, F] = {

    def sendWarningIfNeeded(message: Message): F[Unit] = systemDateTime.now.flatMap(now =>
      if(23 <= now.getHour || now.getHour <= 2)
        sender.tell(message.chat.id, "Время около нуля, обратите внимание при выборе даты")
      else
        Monad[F].unit
    )

    {
      case HasMessage(message@ContainsText("/in")) =>
        chatStateRepository.set(message.chat.id, InControlWaitingForText) >>
        sender.tell(message.chat.id, "Введите напоминание:")

      case (s, message@ContainsText("/exit")) if s != Nop =>
        chatStateRepository.set(message.chat.id, Nop) >>
        sender.tell(message.chat.id, "Создание напоминания отменено")

      case (InControlWaitingForText, message@ContainsText(text)) =>
        chatStateRepository.set(message.chat.id, InControlWaitingForTime(message.chat.id, text)) >>
        sendWarningIfNeeded(message) >>
        sender.tell(message.chat.id, "Введите желаемое время:")

      case (InControlWaitingForTime(_, text), message@ContainsText(time)) =>
        notificationChatService.tryStore(user, message.chat.id, text, time);

      case HasMessage(message@ContainsText(CommandWithQuotedArgs("/in", text :: TailAsText(notification)))) =>
        notificationChatService.tryStore(user, message.chat.id, text, notification)

      case (InControlWaitingForTextEdit(nId), message@ContainsText(text)) =>
        notificationsRepository.update(nId, text) >>
        chatStateRepository.set(message.chat.id, Nop) >>
        sender.tell(message.chat.id, "Текст напоминания изменен")

    }
  }

}
