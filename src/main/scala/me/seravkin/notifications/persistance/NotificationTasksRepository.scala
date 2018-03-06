package me.seravkin.notifications.persistance

import java.sql.Timestamp
import me.seravkin.notifications.domain.Notifications.NotificationTask

trait NotificationTasksRepository[F[_]] {
  def active(now: Timestamp): F[List[NotificationTask]]
}
