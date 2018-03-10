package me.seravkin.notifications.bot

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats._
import cats.data.OptionT
import cats.syntax.all._
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime}
import me.seravkin.notifications.domain._
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.Bot
import me.seravkin.notifications.infrastructure.messages.{Message, Sender}
import me.seravkin.notifications.infrastructure.messages.Message._
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, UsersRepository}

object NotificationBot {

  sealed trait ChatState

  final case object Nop extends ChatState
  final case object InControlWaitingForText extends ChatState
  final case class  InControlWaitingForTime(chatId: Long, text: String) extends ChatState

  final case class NotificationBotBuilder[Msg: Message, F[_]: Monad](
                                       usersRepository: UsersRepository[F],
                                       chatStateRepository: ChatStateRepository[ChatState, F],
                                       sender: Sender[F],
                                       momentInFutureParser: MomentInFutureParser,
                                       notificationsRepository: NotificationsRepository[F],
                                       systemDateTime: SystemDateTime) {

    def build: Bot[Msg, F] =
      message =>
        authenticate(message).flatMap {
          case Some((st, user)) =>
            process(user, st, message)
          case _ if !message.isPrivate =>
            sender.send(message.chatId, "Бот поддерживает только приватные беседы ")
          case _ =>
            sender.send(message.chatId, "Пользователь не аутентифицирован для данного сервиса")
        }


    private[this] def authenticate(msg: Msg): F[Option[(ChatState, User)]] = (for(
      name  <- OptionT.fromOption[F](msg.username);
      user  <- OptionT(usersRepository(name));
      _     <- OptionT.liftF(usersRepository.setChatIdIfNeeded(user, msg.chatId));
      state <- OptionT.liftF(chatStateRepository.get())
    ) yield (state, user)).value

    private[this] def process(user: User, chatState: ChatState, message: Msg): F[Unit] = (chatState, message) match {

      case (Nop, ContainsText("/help")) =>
        sender.send(message.chatId, "Бот c напоминаниями\n" +
          "/in - Напоминает о событии через заданный интервал времени\n" +
          "/show - Показывает активные напоминания\n" +
          "/delete <id> - Удаляет напоминания с указанным id")

      case (Nop, ContainsText("/show")) =>
        for (notifications <- notificationsRepository(user);
             answer        =  show(notifications);
             _             <- sender.send(message.chatId, answer))
          yield ()

      case (Nop, ContainsText(CommandWithArgs("/delete", IsLong(id) :: Nil))) =>
        notificationsRepository.deactivate(id :: Nil) >>
          sender.send(message.chatId, "Напоминание удалено")

      case (Nop, ContainsText("/in")) =>
        chatStateRepository.set(InControlWaitingForText) >>
          sender.send(message.chatId, "Введите напоминание:")

      case (s, ContainsText("/exit")) if s != Nop =>
        chatStateRepository.set(Nop) >>
          sender.send(message.chatId, "Создание напоминания отменено")

      case (InControlWaitingForText, ContainsText(text)) =>
        chatStateRepository.set(InControlWaitingForTime(message.chatId, text)) >>
          sender.send(message.chatId, "Введите желаемое время:")

      case (s @ InControlWaitingForTime(chatId, text), ContainsText(time)) =>
        for(isSuccess <- tryStore(user, message, text, time);
            _         <- chatStateRepository.set(if(isSuccess) Nop else s))
          yield ()

      case (Nop, ContainsText(CommandWithQuotedArgs("/in", text :: TailAsText(notification)))) =>
        tryStore(user, message, text, notification) >>
          ().pure[F]

      case (Nop, ContainsData(HasNotificationId(id))) =>
        (for(notification <- OptionT(notificationsRepository(id.toLong));
             user         <- OptionT(usersRepository(notification.userId));
             chatId       <- OptionT.fromOption[F](user.chatId);
             _            <- OptionT.liftF(chatStateRepository.set(InControlWaitingForTime(chatId, notification.text)));
             _            <- OptionT.liftF(sender.send(chatId, "Введите желаемое время для переноса напоминания:")))
          yield ()).getOrElse()

      case (_, msg) =>
        sender.send(msg.chatId, "Неизвестная команда")
    }

    private[this] def tryStore(user: User, message: Msg, text: String, notification: String): F[Boolean] = {
      momentInFutureParser.parse(notification) match {
        case Right(momentInFuture) =>
          val time = momentInFuture.toExecutionTime(systemDateTime.now)
          (notificationsRepository += OneTime(0, user.id, text, time, isActive = true)) >>
          sender.send(message.chatId, s"Напоминание поставлено и будет отправлено ${beautify(time)}") >>
          true.pure[F]
        case Left(error) =>
          sender.send(message.chatId, s"Время в неправильном формате. Ошибка: $error") >>
          false.pure[F]
      }
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

    private[this] val HasNotificationId = "notification-([0-9]+)".r

    private[this] def show(notifications: List[Notification]): String =
      "Напоминания:\n" +
        notifications.map(n => s"Напоминание ${n.id} о " + "\"" + n.text + "\"").foldLeft("") { _ + "\n" + _ }

    private[this] val DATE_SHORT = "dd.MM"
    private[this] val TIME_FORMAT = "HH:mm"

  }


}
