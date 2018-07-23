package me.seravkin.notifications.bot

import cats._
import cats.data.OptionT
import cats.implicits._
import info.mukel.telegrambot4s.models.CallbackQuery
import me.seravkin.notifications.bot.commands._
import me.seravkin.notifications.bot.services.NotificationChatService
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.NotificationsRepository
import me.seravkin.tg.adapter.matching._

object InCallbackHandler {
  def apply[F[_]: Monad](user: PersistedUser,
                         notificationsRepository: NotificationsRepository[F],
                         sender: Sender[F],
                         momentInFutureParser: MomentInFutureParser,
                         notificationChatService: NotificationChatService[F],
                         systemDateTime: SystemDateTime) : BotHandler[CallbackQuery, F] = {


    case (InControlWaitingForConfirmation(chatId, text, time), ContainsData(SelectNotificationDate(day, month))) =>
      // Можем это делать, т.к. в прошлый раз уже распарсили
      val Right(moment) = momentInFutureParser.parseMomentInFuture(time)

      notificationChatService
        .storeAndReply(user, chatId,
          text, moment.toExecutionTime(systemDateTime.now.withMonth(month).withDayOfMonth(day)))


  }

}
