package me.seravkin.notifications.bot

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import cats._
import cats.data.OptionT
import cats.syntax.all._
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime, Recurrent}
import me.seravkin.notifications.domain._
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.Bot
import me.seravkin.notifications.infrastructure.messages.{Button, Message, Sender}
import me.seravkin.notifications.infrastructure.messages.Message._
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, Page, UsersRepository}
import me.seravkin.notifications.bot.commands._

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
            sender.send(message.chatId, "Бот поддерживает только приватные беседы ") >>
            ignore
          case _ =>
            sender.send(message.chatId, "Пользователь не аутентифицирован для данного сервиса") >>
            ignore
        }


    private[this] val ignore = ().pure[F]

    private[this] def authenticate(msg: Msg): F[Option[(ChatState, User)]] = (for(
      name  <- OptionT.fromOption[F](msg.username);
      user  <- OptionT(usersRepository(name));
      _     <- OptionT.liftF(usersRepository.setChatIdIfNeeded(user, msg.chatId));
      state <- OptionT.liftF(chatStateRepository.get())
    ) yield (state, user)).value

    private[this] def process(user: User, chatState: ChatState, message: Msg): F[Unit] = (chatState, message) match {

      case HasMessage(ContainsText("/help")) =>
        sender.send(message.chatId, HELP_TEXT) >>
        ignore

      case HasMessage(ContainsText("/start")) =>
        sender.send(message.chatId, HELP_TEXT) >>
        ignore

      case HasMessage(ContainsText("/show")) =>
        for (notifications <- notificationsRepository(user);
             answer        =  show(notifications);
             _             <- sender.send(message.chatId, answer))
          yield ()

      case HasMessage(ContainsText(CommandWithArgs("/delete", IsLong(id) :: Nil))) =>
        notificationsRepository.deactivate(id :: Nil) >>
        sender.send(message.chatId, "Напоминание удалено") >>
        ignore

      case HasMessage(ContainsText(CommandWithArgs("/change", IsLong(id) :: Nil))) =>
        changeNotificationDate(message, id)

      case HasMessage(ContainsText(CommandWithArgs("/list", Nil))) =>
        showPage(user, message, 0, 3)

      case HasMessage(ContainsData(ChangePage(id, skip, take))) =>
        editPage(id, user, message, skip, take)

      case HasMessage(ContainsText("/in")) =>
        chatStateRepository.set(InControlWaitingForText) >>
        sender.send(message.chatId, "Введите напоминание:") >>
        ignore

      case (s, ContainsText("/exit")) if s != Nop =>
        chatStateRepository.set(Nop) >>
        sender.send(message.chatId, "Создание напоминания отменено") >>
        ignore

      case (InControlWaitingForText, ContainsText(text)) =>
        chatStateRepository.set(InControlWaitingForTime(message.chatId, text)) >>
        sender.send(message.chatId, "Введите желаемое время:") >>
        ignore

      case (s @ InControlWaitingForTime(chatId, text), ContainsText(time)) =>
        for(isSuccess <- tryStore(user, message, text, time);
            _         <- chatStateRepository.set(if(isSuccess) Nop else s))
          yield ()

      case HasMessage(ContainsData(OpenNotificationMenu(msgId, notificationId, commandToReturn))) =>
        (for(notification <- OptionT(notificationsRepository(notificationId));
             _            <- OptionT.liftF(sender.send(message.chatId, s"Редактирование: ${notification.text}",
              Button("Назад", commandToReturn) ::
              Button("Перенести", ChangeNotificationTimeAndMenu(msgId, notificationId, commandToReturn)) ::
              Button("Удалить", DeleteNotification(msgId, notificationId, commandToReturn)) :: Nil, Some(msgId))))
          yield ()).getOrElseF(ignore)

      case HasMessage(ContainsText(CommandWithQuotedArgs("/in", text :: TailAsText(notification)))) =>
        tryStore(user, message, text, notification) >>
        ignore

      case HasMessage(ContainsData(DeleteNotification(msgId, notificationId, ChangePage(_, skip, take)))) =>
        for(_ <- notificationsRepository.deactivate(notificationId :: Nil);
            _ <- editPage(msgId, user, message, skip, take))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationTimeAndMenu(msgId, notificationId, _))) =>
        for(_ <- notificationsRepository.deactivate(notificationId :: Nil);
            _ <- changeNotificationDate(message, notificationId))
          yield ()

      case HasMessage(ContainsData(ChangeNotificationTime(id))) =>
        changeNotificationDate(message, id)

      case (_, msg) =>
        sender.send(msg.chatId, "Неизвестная команда") >>
        ignore
    }

    private[this] def changeNotificationDate(message: Msg, id: Long) = {
      (for (notification <- OptionT(notificationsRepository(id));
            user         <- OptionT(usersRepository(notification.userId));
            chatId       <- OptionT.fromOption[F](user.chatId);
            _            <- OptionT.liftF(chatStateRepository.set(InControlWaitingForTime(chatId, notification.text)));
            _            <- OptionT.liftF(sender.send(chatId, "Введите желаемое время для переноса напоминания:")))
        yield ()).getOrElseF(sender.send(message.chatId, s"Напоминание с id $id не найдено") >> ignore)
    }

    private[this] def tryStore(user: User, message: Msg, text: String, notification: String): F[Boolean] = {
      momentInFutureParser.parseMomentInFuture(notification) match {
        case Right(momentInFuture) =>
          val time = momentInFuture.toExecutionTime(systemDateTime.now)
          for(_ <- notificationsRepository += OneTime(0, user.id, text, time, isActive = true);
              _ <- sender.send(message.chatId, s"Напоминание поставлено и будет отправлено ${beautify(time)}"))
            yield true
        case Left(error) =>
          for(_ <- sender.send(message.chatId, s"Время в неправильном формате. Ошибка: $error"))
            yield false
      }
    }


    private[this] def editPage(id: Int, user: User, message: Msg, skip: Int, take: Int): F[Unit] =
      for(notifications <- notificationsRepository(user, skip, take);
          answer        =  show(notifications.contents);
          _             <- sender.send(message.chatId, answer, pageToButtons(id, skip, take, notifications), Some(id)))
        yield ()

    private[this] def showPage(user: User, message: Msg, skip: Int, take: Int): F[Unit] =
      for(notifications <- notificationsRepository(user, skip, take);
          answer        =  show(notifications.contents);
          msgId         <- sender.send(message.chatId, answer);
          _             <- sender.send(message.chatId, answer, pageToButtons(msgId, skip, take, notifications), Some(msgId)))
        yield ()

    private[this] def normalize(i: Int): Int = if(i < 0) 0 else i

    private[this] def pageToButtons[T <: Notification](id: Int, skip: Int, take: Int, page: Page[T]): List[Button] =
      (if(page.hasPrevious) List(Button("<-", ChangePage(id,normalize(skip - take), take))) else List.empty) ++
      page.contents.zipWithIndex.map { case (x,i) => Button((i + 1).toString, OpenNotificationMenu(id,
        x.id, ChangePage(id, skip, take))) } ++
      (if(page.hasNext) List(Button("->", ChangePage(id, skip + take, take))) else List.empty)

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


    private[this] def show(notifications: List[Notification]): String =
      "Напоминания:\n" +
        notifications.collect {
          case n: OneTime => s"Напоминание ${n.id} о " + "\"" + n.text + "\" в " + n.when.toString
          case n: Recurrent => s"Напоминание ${n.id} о " + "\"" + n.text + "\""
        }.foldLeft("") { _ + "\n" + _ }

    private[this] val DATE_SHORT = "dd.MM"
    private[this] val TIME_FORMAT = "HH:mm"
    private[this] val HELP_TEXT = "Бот c напоминаниями\n" +
      "/in - Напоминает о событии через заданный интервал времени\n" +
      "/show - Показывает активные напоминания\n" +
      "/delete <id> - Удаляет напоминания с указанным id\n" +
      "/change <id> - Изменяет дату и время на напоминании с указанным id"


    private object HasMessage {
      def unapply(arg: (ChatState, Msg)): Option[Msg] = arg match {
        case (Nop, msg) => Some(msg)
        case _ => None
      }
    }

    private object HasState {
      def unapply(arg: (ChatState, Msg)): Option[(ChatState, Msg)] =
        Some(arg)
    }

  }


}
