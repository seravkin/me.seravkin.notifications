package me.seravkin.notifications.persistance.botio

import java.sql.Timestamp

import doobie._
import doobie.implicits._
import me.seravkin.notifications.domain.Notifications
import me.seravkin.notifications.domain.Notifications.NotificationTask
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.persistance.{NotificationTasksRepository, botio}


object DoobieNotificationTasksRepository extends NotificationTasksRepository[BotIO] with BotIORepository {
  override def active(now: Timestamp): BotIO[List[Notifications.NotificationTask]] = botIO {
    sql"""SELECT n.id, u.chat_id, n.text FROM notifications n
          JOIN users u ON n.id_user = u.id
          WHERE n.is_active = TRUE AND $now > n.dt_to_notificate
       """.query[NotificationTask]
      .stream
      .compile
      .toList
  }
}
