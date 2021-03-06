package me.seravkin.notifications.bot.services

import cats._
import cats.data._
import cats.implicits._
import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.bot.ChatState.{InControlWaitingForTime, Nop}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.interpreter._
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, UsersRepository}

final class NotificationChatServiceImpl[F[_]: Monad](notificationsRepository: NotificationsRepository[F],
                                                     usersRepository: UsersRepository[F],
                                                     chatStateRepository: ChatStateRepository[ChatState, F],
                                                     momentInFutureParser: MomentInFutureParser[DatesFactory[F, Dates]],
                                                     systemDateTime: SystemDateTime[F],
                                                     timeBeautifyService: TimeBeautifyService[F],
                                                     sender: Sender[F]) extends NotificationChatService[F] {

  override def changeNotificationDate(chatId: Long, notificationId: Long): F[Unit] = {
    (for (notification <- OptionT(notificationsRepository(notificationId));
          user         <- OptionT(usersRepository(notification.userId));
          chatId       <- OptionT.fromOption[F](user.chatId);
          _            <- OptionT.liftF(chatStateRepository.set(chatId, InControlWaitingForTime(chatId, notification.text)));
          _            <- OptionT.liftF(sender.tell(chatId, "Введите желаемое время для переноса напоминания:")))
      yield ()).getOrElseF(sender.tell(chatId, s"Напоминание с id $notificationId не найдено"))
  }

  override def tryStore(user: PersistedUser, chatId: Long, text: String, notification: String): F[Unit] =
    tryParseAndInterpret(notification).value.flatMap {
      case Right(dates) =>
        storeAndReply(user, chatId, text, dates)
      case Left(error) =>
        sender.tell(chatId,  s"Время в неправильном формате. Ошибка: $error")
    }


  private[this] def tryParseAndInterpret(text: String): EitherT[F, String, Dates] = {
    for (factory <- EitherT.fromEither[F](momentInFutureParser.parseMomentInFuture(text));
         now     <- EitherT.liftF(systemDateTime.now);
         result  <- factory(now))
      yield result
  }

  override def storeAndReply(user: PersistedUser, chatId: Long, text: String, time: Dates): F[Unit] = {
    for (_ <- notificationsRepository += Notification(0, user.id, text, isActive = true, time);
         s <- timeBeautifyService.beautify(time);
         _ <- sender.tell(chatId, s"Напоминание поставлено и будет отправлено $s");
         _ <- chatStateRepository.set(chatId, Nop))
      yield ()
  }


}
