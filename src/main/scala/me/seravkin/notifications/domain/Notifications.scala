package me.seravkin.notifications.domain

import java.time.{Duration, LocalDateTime}

import me.seravkin.notifications.domain.interpreter.Dates

object Notifications {

  final case class Notification(id: Long,
                                userId: Long,
                                text: String,
                                isActive: Boolean,
                                dates: Dates)

  final case class NotificationTask(id: Long, chatId: Long, text: String)
}
