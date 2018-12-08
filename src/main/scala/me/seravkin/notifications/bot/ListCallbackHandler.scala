package me.seravkin.notifications.bot

import cats._
import cats.data._
import cats.implicits._
import com.bot4s.telegram.models.CallbackQuery
import me.seravkin.notifications.bot.ChatState.InControlWaitingForTextEdit
import me.seravkin.notifications.bot.commands._
import me.seravkin.notifications.bot.services.{NotificationChatService, PageView}
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.persistance.NotificationsRepository
import me.seravkin.tg.adapter.matching.ContainsData

object ListCallbackHandler {

  def apply[F[_]: Monad](user: PersistedUser, pageView: PageView[F],
                         notificationsRepository: NotificationsRepository[F],
                         notificationChatService: NotificationChatService[F],
                         chatStateRepository: ChatStateRepository[ChatState, F],
                         sender: Sender[F]): BotHandler[CallbackQuery, F] = hasChatId { chatId => {

      case HasMessage(ContainsData(ChangePage(id, skip, take))) =>
        pageView.editPage(id, user, chatId, skip, take)

      case HasMessage(ContainsData(DeleteNotification(msgId, notificationId, ChangePage(_, skip, take)))) =>
        for (_ <- notificationsRepository.deactivate(notificationId :: Nil);
             _ <- pageView.editPage(msgId, user, chatId, skip, take))
          yield ()

      case HasMessage(ContainsData(DeleteNotification(_, notificationId, _))) =>
        notificationsRepository.deactivate(notificationId :: Nil)

      case HasMessage(ContainsData(ChangeNotificationTimeAndMenu(_, notificationId, _))) =>
        for (_ <- notificationsRepository.deactivate(notificationId :: Nil);
             _ <- notificationChatService.changeNotificationDate(chatId, notificationId))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationText(_, notificationId, _))) =>
        for(_ <- chatStateRepository.set(chatId, InControlWaitingForTextEdit(notificationId));
            _ <- sender.ask(chatId, "Введите желаемый текст для напоминания:"))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationTime(id))) =>
        notificationChatService.changeNotificationDate(chatId, id)

      case HasMessage(message@ContainsData(OpenNotificationMenu(msgId, notificationId, commandToReturn))) =>
        (for (notification <- OptionT(notificationsRepository(notificationId));
              id           <- OptionT.fromOption[F](message.message.map(_.chat.id));
              _            <- OptionT.liftF(sender.ask(id, s"Редактирование: ${notification.text}",
                Button("Назад", commandToReturn) ::
                  Button("Перенести", ChangeNotificationTimeAndMenu(msgId, notificationId, commandToReturn)) ::
                  Button("Изменить", ChangeNotificationText(msgId, notificationId, commandToReturn)) ::
                  Button("Удалить", DeleteNotification(msgId, notificationId, commandToReturn)) :: Nil, Some(msgId))))
          yield ()).getOrElseF(Monad[F].unit)
    }
  }
}
