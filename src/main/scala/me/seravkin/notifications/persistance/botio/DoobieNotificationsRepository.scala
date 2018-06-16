package me.seravkin.notifications.persistance.botio

import java.time.LocalDateTime

import doobie._
import doobie.implicits._
import cats._
import cats.implicits._
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime}
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.domain.{Notifications, User}
import me.seravkin.notifications.persistance.{NotificationsRepository, Page, botio}


object DoobieNotificationsRepository extends NotificationsRepository[BotIO] with BotIORepository {

  private implicit val DateTimeMeta: Meta[LocalDateTime] =
    Meta[java.sql.Timestamp].xmap(
      ts => ts.toLocalDateTime,
      dt => java.sql.Timestamp.valueOf(dt)
    )

  override def apply(id: Long): BotIO[Option[Notifications.Notification]] = botIO {
    sql"SELECT id, id_user, text, dt_to_notificate, is_active FROM notifications WHERE id = $id"
      .read[OneTime]
      .last
      .map(_.map(_.asInstanceOf[Notification]))
  }


  override def apply(user: User): BotIO[List[Notifications.Notification]] = botIO {
    sql"SELECT id, id_user, text, dt_to_notificate, is_active FROM notifications WHERE is_active = TRUE AND id_user = ${user.id}"
      .read[OneTime]
      .toList
      .map(_.map(_.asInstanceOf[Notification]))
  }

  override def +=[T <: Notifications.Notification](t: T): BotIO[T] = botIO {
    t match {
      case o@OneTime(_, userId, text, when, isActive) =>
        sql"INSERT INTO notifications (text, is_active, dt_to_notificate, id_user) VALUES ($text,TRUE, $when, $userId) RETURNING id"
          .update
          .withGeneratedKeys[Long]("id")
          .compile
          .last
          .map(_.get)
          .map(id => o.copy(id = id).asInstanceOf[T])
    }
  }

  override def deactivate(ids: List[Long]): BotIO[Unit] = botIO {
    ids match {
      case any :: xs =>
        (fr"UPDATE notifications set is_active = FALSE WHERE " ++ Fragments.in(fr"id", ids.toNel.get))
          .update
          .run
          .map(x => ())
      case _ => ().pure[ConnectionIO]
    }
  }

  private[this] def activeNotificationsCount(user: User): BotIO[Int] = botIO {
    sql"SELECT count(id) from notifications where is_active = TRUE AND id_user = ${user.id}"
      .query[Int]
      .unique
  }

  private[this] def pageOfNotifications(user: User, skip: Int, take: Int): BotIO[List[OneTime]] = botIO {
    sql"SELECT id, id_user, text, dt_to_notificate, is_active FROM notifications WHERE is_active = TRUE AND id_user = ${user.id} ORDER BY dt_to_notificate DESC LIMIT $take OFFSET $skip"
      .read[OneTime]
      .toList
  }

  override def apply(user: User, skip: Int, take: Int): BotIO[Page[Notification]] = for(
    count   <- activeNotificationsCount(user);
    entries <- pageOfNotifications(user, skip, take)
  ) yield Page(entries, skip != 0, count > skip + entries.length)

}
