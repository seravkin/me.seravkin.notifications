package me.seravkin.notifications.domain.services


import cats._
import cats.instances.list._
import cats.syntax.all._
import me.seravkin.notifications.bot.commands.{ChangeNotificationTime, DeleteNotification}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.NotificationsRepository

case class NotificationTasksServiceImpl[F[_]: Monad](systemDateTime: SystemDateTime,
                                                     notificationsRepository: NotificationsRepository[F],
                                                     sender: Sender[F]) extends NotificationTasksService[F] {

  private[this] def sendTask(chatId: Long, notification: Notification): F[Unit] =
    sender.tell(
      chatId,
      notification.text,
      notificationToButtons(notification))

  private[this] def notificationToButtons(notification: Notification): List[Button] =
    Button("Перенести", ChangeNotificationTime(notification.id)) ::
      (notification.dates match {

        case _: OneDate => Nil
        case _ => Button("Отменить", DeleteNotification(0, notification.id, "")) :: Nil
      })

  //TODO: move to another service
  def next(notification: Notification): Option[Notification] = notification.dates match {
    case c: Confirmation => c.next(systemDateTime.now).map(d => notification.copy(dates = d))
    case d: OneDate => d.next(systemDateTime.now).map(d => notification.copy(dates = d))
    case p: Periodic => p.next(systemDateTime.now).map(d => notification.copy(dates = d))
  }

  def actionsFor(notifications: List[Notification]): (List[Long], List[Notification]) = {
    val (toDeactivate, toSave) = notifications.map(n => (n, next(n))).partition(_._2.isEmpty)

    (toDeactivate.map(_._1.id), toSave.collect { case (_, Some(c)) => c })
  }

  override def sendNotificationsIfNeeded(): F[Unit] = for(
    tasks     <- notificationsRepository.active(systemDateTime.now);
    chats     =  tasks.map { case (id, x) => (x.id, id) }.toMap;
    (ids, ns) =  actionsFor(tasks.map(_._2));
    _         <- tasks.map { case (chatId, x) => sendTask(chatId, x) }.sequence;
    _         <- notificationsRepository.update(ns.map(x => (x.id, x.dates.notificationDate)));
    _         <- notificationsRepository.deactivate(ids)
  ) yield ()
}
