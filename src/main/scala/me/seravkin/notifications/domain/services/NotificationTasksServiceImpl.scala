package me.seravkin.notifications.domain.services

import java.sql.Timestamp

import cats._
import cats.instances.list._
import cats.syntax.all._
import me.seravkin.notifications.bot.commands.ChangeNotificationTime
import me.seravkin.notifications.domain.Notifications.NotificationTask
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationTasksRepository, NotificationsRepository}

case class NotificationTasksServiceImpl[F[_]: Monad](systemDateTime: SystemDateTime,
                                                     notificationTasksRepository: NotificationTasksRepository[F],
                                                     notificationsRepository: NotificationsRepository[F],
                                                     sender: Sender[F]) extends NotificationTasksService[F] {

  private[this] def sendTask(notificationTask: NotificationTask): F[Unit] =
    sender.send(
      notificationTask.chatId,
      notificationTask.text,
      Button("Перенести", ChangeNotificationTime(notificationTask.id)) :: Nil) >> ().pure[F]

  override def sendNotificationsIfNeeded(): F[Unit] = for(
    tasks <- notificationTasksRepository.active(Timestamp.valueOf(systemDateTime.now));
    ids   =  tasks.map(_.id);
    _     <- tasks.map(sendTask).sequence;
    _     <- notificationsRepository.deactivate(ids)
  ) yield ()
}
