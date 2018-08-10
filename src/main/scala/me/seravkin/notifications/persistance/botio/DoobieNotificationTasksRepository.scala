package me.seravkin.notifications.persistance.botio

import java.sql.Timestamp

import cats.Monad
import doobie._
import doobie.implicits._
import me.seravkin.notifications.domain.Notifications
import me.seravkin.notifications.domain.Notifications.NotificationTask
import me.seravkin.notifications.infrastructure.BotF
import me.seravkin.notifications.persistance.{NotificationTasksRepository, botio}


final class DoobieNotificationTasksRepository[F[_]: Monad] extends NotificationTasksRepository[BotF[F, ?]] with BotIORepository[F] {
  override def active(now: Timestamp): BotF[F, List[Notifications.NotificationTask]] = botIO {
    sql"""SELECT n.id, u.chat_id, n.text FROM notifications n
          JOIN users u ON n.id_user = u.id
          WHERE n.is_active = TRUE AND $now > n.dt_to_notificate
       """
      .read[NotificationTask]
      .toList
  }
}
