package me.seravkin.notifications.bot

import cats._
import cats.data._
import cats.implicits._
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.bot.NotificationBot._
import me.seravkin.notifications.bot.commands.{ChangeNotificationTimeAndMenu, DeleteNotification, OpenNotificationMenu, SelectNotificationDate}
import me.seravkin.notifications.bot.services.NotificationChatService
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.messages.Message.{CommandWithQuotedArgs, TailAsText}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.persistance.NotificationsRepository
import me.seravkin.tg.adapter.matching._

object InMessageHandler {
  def apply[F[_]: Monad](user: PersistedUser,
                         chatStateRepository: ChatStateRepository[ChatState, F],
                         notificationChatService: NotificationChatService[F],
                         notificationsRepository: NotificationsRepository[F],
                         sender: Sender[F]): BotHandler[Message, F] = {

    case HasMessage(message@ContainsText("/in")) =>
      chatStateRepository.set(message.chat.id, InControlWaitingForText) >>
        sender.ask(message.chat.id, "Введите напоминание:") >>
        Monad[F].unit

    case (s, message@ContainsText("/exit")) if s != Nop =>
      chatStateRepository.set(message.chat.id, Nop) >>
      sender.ask(message.chat.id, "Создание напоминания отменено") >>
      Monad[F].unit

    case (InControlWaitingForText, message@ContainsText(text)) =>
      chatStateRepository.set(message.chat.id, InControlWaitingForTime(message.chat.id, text)) >>
      sender.ask(message.chat.id, "Введите желаемое время:") >>
      Monad[F].unit

    case (s@InControlWaitingForTime(chatId, text), message@ContainsText(time)) =>
      notificationChatService.tryStore(user, message.chat.id, text, time);

    case HasMessage(message@ContainsText(CommandWithQuotedArgs("/in", text :: TailAsText(notification)))) =>
      notificationChatService.tryStore(user, message.chat.id, text, notification)

    case (InControlWaitingForTextEdit(nId), message@ContainsText(text)) =>
      notificationsRepository.update(nId, text) >>
      chatStateRepository.set(message.chat.id, Nop) >>
      sender.ask(message.chat.id, "Текст напоминания изменен") >>
      Monad[F].unit

  }

}
