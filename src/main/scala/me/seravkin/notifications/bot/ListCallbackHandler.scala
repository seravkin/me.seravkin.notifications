package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import info.mukel.telegrambot4s.models.CallbackQuery
import me.seravkin.notifications.bot.commands.{ChangeNotificationTime, ChangeNotificationTimeAndMenu, ChangePage, DeleteNotification}
import me.seravkin.notifications.bot.services.{NotificationChatService, PageView}
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.persistance.NotificationsRepository
import me.seravkin.tg.adapter.matching.ContainsData

object ListCallbackHandler {

  def apply[F[_]: Monad](user: PersistedUser, pageView: PageView[F],
                         notificationsRepository: NotificationsRepository[F],
                         notificationChatService: NotificationChatService[F]): BotHandler[CallbackQuery, F] = hasChatId { chatId => {

      case HasMessage(ContainsData(ChangePage(id, skip, take))) =>
        pageView.editPage(id, user, chatId, skip, take)

      case HasMessage(ContainsData(DeleteNotification(msgId, notificationId, ChangePage(_, skip, take)))) =>
        for (_ <- notificationsRepository.deactivate(notificationId :: Nil);
             _ <- pageView.editPage(msgId, user, chatId, skip, take))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationTimeAndMenu(msgId, notificationId, _))) =>
        for (_ <- notificationsRepository.deactivate(notificationId :: Nil);
             _ <- notificationChatService.changeNotificationDate(chatId, notificationId))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationTime(id))) =>
        notificationChatService.changeNotificationDate(chatId, id)
    }
  }
}
