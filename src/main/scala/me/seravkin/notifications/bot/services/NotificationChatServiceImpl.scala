package me.seravkin.notifications.bot.services

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats._
import cats.implicits._
import cats.data._
import me.seravkin.notifications.bot.{ChatState, InControlWaitingForConfirmation, InControlWaitingForTime, Nop}
import me.seravkin.notifications.bot.commands.SelectNotificationDate
import me.seravkin.notifications.domain.Notifications.OneTime
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.interpreter.NotificationPrototype
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, UsersRepository}

final class NotificationChatServiceImpl[F[_]: Monad](notificationsRepository: NotificationsRepository[F],
                                                     usersRepository: UsersRepository[F],
                                                     chatStateRepository: ChatStateRepository[ChatState, F],
                                                     momentInFutureParser: MomentInFutureParser[NotificationPrototype[F]],
                                                     systemDateTime: SystemDateTime,
                                                     sender: Sender[F]) extends NotificationChatService[F] {

  override def changeNotificationDate(chatId: Long, notificationId: Long): F[Unit] = {
    (for (notification <- OptionT(notificationsRepository(notificationId));
          user         <- OptionT(usersRepository(notification.userId));
          chatId       <- OptionT.fromOption[F](user.chatId);
          _            <- OptionT.liftF(chatStateRepository.set(chatId, InControlWaitingForTime(chatId, notification.text)));
          _            <- OptionT.liftF(sender.tell(chatId, "Введите желаемое время для переноса напоминания:")))
      yield ()).getOrElseF(sender.tell(chatId, s"Напоминание с id $notificationId не найдено"))
  }

  override def tryStore(user: PersistedUser, chatId: Long, text: String, notification: String): F[Unit] = {
    momentInFutureParser.parseMomentInFuture(notification) match {

      case Right(proto @ NotificationPrototype(_, true, _)) if isUncertainTime =>
        val delta = if(systemDateTime.now.getHour < 12) -1 else 0

        for (today    <- proto(systemDateTime.now.plusDays(delta));
             tomorrow <- proto(systemDateTime.now.plusDays(1 + delta));
             _        <- sender.tell(chatId, "Какая дата точно имелась в виду:", dateButton(today) :: dateButton(tomorrow) :: Nil);
             _        <- chatStateRepository.set(chatId, InControlWaitingForConfirmation(chatId, text, notification)))
          yield ()

      case Right(proto) =>
        for(time <- proto(systemDateTime.now);
            _    <- storeAndReply(user, chatId, text, time))
          yield ()

      case Left(error) =>
        for (_ <- sender.ask(chatId, s"Время в неправильном формате. Ошибка: $error"))
          yield ()
    }
  }

  private[this] def dateButton(day: LocalDateTime) = Button(
    day.format(DateTimeFormatter.ofPattern("EEEE dd.MM", new Locale("ru"))),
    SelectNotificationDate(day.getDayOfMonth, day.getMonthValue)
  )

  private[this] def isUncertainTime =
    systemDateTime.now.getHour >= 23 && systemDateTime.now.getMinute >= 55 || systemDateTime.now.getHour <= 3

  override def storeAndReply(user: PersistedUser, chatId: Long, text: String, time: LocalDateTime): F[Unit] = {
    for (_ <- notificationsRepository += OneTime(0, user.id, text, time, isActive = true);
         _ <- sender.ask(chatId, s"Напоминание поставлено и будет отправлено ${beautify(time)}");
         _ <- chatStateRepository.set(chatId, Nop))
      yield ()
  }

  private[this] def beautify(time: LocalDateTime) = {
    val formattedTime = time.format(DateTimeFormatter.ofPattern(TIME_FORMAT))

    val date =
      if (systemDateTime.now.getDayOfYear != time.getDayOfYear)
        time.format(DateTimeFormatter.ofPattern(DATE_SHORT))
      else
        ""

    val dateWhiteSpace = if (date.isEmpty) "" else date + " "

    dateWhiteSpace + "в " + formattedTime
  }

  private[this] val DATE_SHORT = "dd.MM"
  private[this] val TIME_FORMAT = "HH:mm"

}
