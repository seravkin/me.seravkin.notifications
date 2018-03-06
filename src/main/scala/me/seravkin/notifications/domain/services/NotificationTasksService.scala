package me.seravkin.notifications.domain.services

trait NotificationTasksService[F[_]] {
  def sendNotificationsIfNeeded(): F[Unit]
}
