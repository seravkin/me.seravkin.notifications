package me.seravkin.notifications.bot.services

import java.time.LocalDateTime

import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.interpreter.Dates

trait NotificationChatService[F[_]] {

  def changeNotificationDate(chatId: Long, notificationId: Long): F[Unit]

  def tryStore(user: PersistedUser, chatId: Long, text: String, notification: String): F[Unit]

  def storeAndReply(user: PersistedUser, chatId: Long, text: String, time: Dates): F[Unit]

}
