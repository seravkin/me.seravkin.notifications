package me.seravkin.notifications.bot.services

import cats._
import cats.implicits._
import me.seravkin.notifications.bot._
import me.seravkin.notifications.bot.commands.{ChangePage, OpenNotificationMenu}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.persistance.{NotificationsRepository, Page}

final class PageViewImpl[F[_]: Monad](notificationsRepository: NotificationsRepository[F],
                               sender: Sender[F]) extends PageView[F] {

  private[this] def normalize(i: Int): Int = if (i < 0) 0 else i

  private[this] def pageToButtons[T <: Notification](id: Int, skip: Int, take: Int, page: Page[T]): List[Button] =
    (if (page.hasPrevious) List(Button("<-", ChangePage(id, normalize(skip - take), take))) else List.empty) ++
      page.contents.zipWithIndex.map { case (x, i) => Button((i + 1).toString, OpenNotificationMenu(id,
        x.id, ChangePage(id, skip, take)))
      } ++
      (if (page.hasNext) List(Button("->", ChangePage(id, skip + take, take))) else List.empty)

  override def editPage(id: Int, user: PersistedUser, chatId: Long, skip: Int, take: Int): F[Unit] =
    for (notifications <- notificationsRepository(user, skip, take);
         answer        =  notifications.contents.show;
         _             <- sender.ask(chatId, answer, pageToButtons(id, skip, take, notifications), Some(id)))
      yield ()

  override def showPage(user: PersistedUser, chatId: Long, skip: Int, take: Int): F[Unit] =
    for (notifications <- notificationsRepository(user, skip, take);
         answer        =  notifications.contents.show;
         msgId         <- sender.ask(chatId, answer);
         _             <- sender.ask(chatId, answer, pageToButtons(msgId, skip, take, notifications), Some(msgId)))
      yield ()

}
