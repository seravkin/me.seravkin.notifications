package me.seravkin.notifications.persistance.botio

import java.time.{Duration, LocalDateTime}
import cats._
import cats.effect.Bracket
import cats.implicits._
import doobie._
import doobie.implicits._
import me.seravkin.notifications.domain.Notifications._
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.domain.{Notifications, PersistedUser}
import me.seravkin.notifications.infrastructure.BotF
import me.seravkin.notifications.persistance.{NotificationsRepository, Page}


final class DoobieNotificationsRepository[F[_]: Monad: Bracket[*[_], Throwable]] extends NotificationsRepository[BotF[F, *]] with BotIORepository[F] {

  private[this] case class NotificationFlatten(id: Long, userId: Long, text: String, kind: String,
                                               isActive: Boolean,
                                               dateToNotificate: Option[LocalDateTime] = None,
                                               periodInSeconds: Option[Long] = None,
                                               hour: Option[Int] = None,
                                               minute: Option[Int] = None,
                                               days: Option[String] = None,
                                               start: Option[LocalDateTime] = None,
                                               end: Option[LocalDateTime] = None,
                                               recurrencyType: Option[String] = None) {



    def toNotification: Option[Notification] = kind match {
      case "OneDate" => dateToNotificate.map(dt => Notification(id, userId, text, isActive, OneDate(dt)))
      case "Confirmation" => dateToNotificate.map2(periodInSeconds) { case (dt, per) =>
        Notification(id, userId, text, isActive, Confirmation(dt, Duration.ofSeconds(per)))
      }
      case "Periodic" => (dateToNotificate, hour, minute, days.map(_.split(',').map(_.toInt).toSet), recurrencyType)
        .mapN { case (dt, h, m, d, r) =>
        Notification(id, userId, text, isActive,
          Periodic(dt, h, m, d, if (r == "Week") RecurrencyType.Week else RecurrencyType.Month, start, end))
      }
    }

  }

  private object NotificationFlatten {
    def apply(nt: Notification): NotificationFlatten = nt.dates match {
      case OneDate(dt) =>
        NotificationFlatten(nt.id, nt.userId, nt.text, "OneDate", nt.isActive, Some(dt))
      case Confirmation(dt, period) =>
        NotificationFlatten(nt.id, nt.userId, nt.text, "Confirmation", nt.isActive,
        Some(dt), Some(period.getSeconds))
      case Periodic(dt, hour, minute, days, recType, start, end) =>
        NotificationFlatten(nt.id, nt.userId, nt.text, "Periodic", nt.isActive, Some(dt), None, Some(hour), Some(minute),
          Some(days.map(_.toString).reduce { _ + "," + _}), start, end, Some(recType match {
            case RecurrencyType.Week => "Week"
            case RecurrencyType.Month => "Month"
          }))
    }
  }

  private implicit val DateTimeMeta: Meta[LocalDateTime] =
    javasql.TimestampMeta.imap(
      ts => ts.toLocalDateTime)(
      dt => java.sql.Timestamp.valueOf(dt)
    )

  override def apply(id: Long): BotF[F, Option[Notifications.Notification]] = botIO {
    sql"SELECT id, id_user, text, kind, is_active, dt_to_notificate, period, hour, minute, days, start, finish, recurrency_type FROM notifications WHERE id = $id"
      .read[NotificationFlatten]
      .last
      .map(_.flatMap(_.toNotification))
  }


  override def apply(user: PersistedUser): BotF[F, List[Notifications.Notification]] = botIO {
    sql"SELECT id, id_user, text, kind, is_active, dt_to_notificate, period, hour, minute, days, start, finish, recurrency_type  FROM notifications WHERE is_active = TRUE AND id_user = ${user.id}"
      .read[NotificationFlatten]
      .toList
      .map(_.flatMap(_.toNotification))
  }

  override def +=[T <: Notifications.Notification](t: T): BotF[F, T] = botIO {
      val flatten = NotificationFlatten(t)

      sql"""INSERT INTO notifications
            (id_user, text, kind, is_active, dt_to_notificate, period, hour, minute, days, start, finish, recurrency_type)
            VALUES (${flatten.userId},${flatten.text},${flatten.kind},TRUE,
                    ${flatten.dateToNotificate}, ${flatten.periodInSeconds}, ${flatten.hour}, ${flatten.minute},
                    ${flatten.days}, ${flatten.start}, ${flatten.end}, ${flatten.recurrencyType}) RETURNING id"""
        .update
        .withGeneratedKeys[Long]("id")
        .compile
        .last
        .map(_.get)
        .map(id => t.copy(id = id).asInstanceOf[T])

  }

  override def deactivate(ids: List[Long]): BotF[F, Unit] = botIO {
    ids match {
      case Nil => ().pure[ConnectionIO]
      case _ =>
        (fr"UPDATE notifications set is_active = FALSE WHERE " ++ Fragments.in(fr"id", ids.toNel.get))
          .update
          .run
          .map(x => ())
    }
  }

  private[this] def activeNotificationsCount(user: PersistedUser): BotF[F, Int] = botIO {
    sql"SELECT count(id) from notifications where is_active = TRUE AND id_user = ${user.id}"
      .query[Int]
      .unique
  }

  private[this] def pageOfNotifications(user: PersistedUser, skip: Int, take: Int): BotF[F, List[Notification]] = botIO {
    sql"""SELECT id, id_user, text, kind, is_active, dt_to_notificate, period, hour, minute, days, start, finish, recurrency_type
          FROM notifications WHERE is_active = TRUE AND id_user = ${user.id}
          ORDER BY dt_to_notificate DESC LIMIT $take OFFSET $skip"""
      .read[NotificationFlatten]
      .toList
      .map(_.map(_.toNotification).collect { case Some(x) => x })
  }


  override def active(now: LocalDateTime): BotF[F, List[(Long, Notification)]] = botIO {
    sql"""SELECT u.chat_id, n.id, n.id_user, n.text, n.kind, n.is_active, n.dt_to_notificate, n.period, n.hour, n.minute, n.days, n.start, n.finish, n.recurrency_type
          FROM notifications n
          JOIN users u ON n.id_user = u.id
          WHERE n.is_active = TRUE AND $now > n.dt_to_notificate"""
      .read[(Long, NotificationFlatten)]
      .toList
      .map(_
        .map { case (chatId, nt) => (chatId, nt.toNotification) }
        .collect { case (chatId, Some(n)) => (chatId, n) })
  }


  override def apply(user: PersistedUser, skip: Int, take: Int): BotF[F, Page[Notification]] = for(
    count   <- activeNotificationsCount(user);
    entries <- pageOfNotifications(user, skip, take)
  ) yield Page(entries, skip != 0, count > skip + entries.length)

  override def update(id: Long, text: String): BotF[F, Unit] = botIO {
    sql"UPDATE notifications SET text = $text WHERE id = $id"
      .update
      .run
      .map(_ => ())
  }

  private[this] def updateDate(id: Long, localDateTime: LocalDateTime): ConnectionIO[Unit] =
    sql"UPDATE notifications SET dt_to_notificate = $localDateTime WHERE id = $id"
    .update
    .run
    .map(_ => ())

  override def update(pairs: List[(Long, LocalDateTime)]): BotF[F, Unit] = botIO {
    pairs
      .map { case (id, ldt) => updateDate(id, ldt) }
      .sequence
      .map(_ => ())
  }
}
