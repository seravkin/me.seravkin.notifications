package me.seravkin.notifications.domain

import java.time.LocalDateTime

object Notifications {
  sealed trait Notification {
    def id: Long
    def userId: Long
    def text: String
    def isActive: Boolean
  }

  final case class OneTime(id: Long, userId: Long, text: String, when: LocalDateTime, isActive: Boolean) extends Notification
  final case class Recurrent(id: Long, userId: Long, text: String, start: LocalDateTime, interval: LocalDateTime, isActive: Boolean) extends Notification

  final case class NotificationTask(id: Long, chatId: Long, text: String)
}
