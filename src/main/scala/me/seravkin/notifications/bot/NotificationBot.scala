package me.seravkin.notifications.bot

import cats._
import cats.data.OptionT
import cats.syntax.all._
import com.bot4s.telegram.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.bot.services.{NotificationChatService, PageView}
import me.seravkin.notifications.domain._
import me.seravkin.notifications.domain.interpreter.{Dates, DatesFactory}
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, UsersRepository}
import me.seravkin.tg.adapter.events._

final case class NotificationBot[F[_] : Monad](usersRepository: UsersRepository[F],
                                               chatStateRepository: ChatStateRepository[ChatState, F],
                                               sender: Sender[F],
                                               momentInFutureParser: MomentInFutureParser[DatesFactory[F, Dates]],
                                               notificationsRepository: NotificationsRepository[F],
                                               notificationChatService: NotificationChatService[F],
                                               pageView: PageView[F],
                                               systemDateTime: SystemDateTime) extends (BotEvent => F[Unit]) {


  def apply(event: BotEvent): F[Unit] = event match {
    case ReceiveMessage(message) if message.chat.`type` == ChatType.Private =>
      authenticateAndProcessMessage(message)
    case ReceiveEditedMessage(message)  if message.chat.`type` == ChatType.Private =>
      authenticateAndProcessMessage(message)
    case ReceiveCallbackQuery(query) if query.message.exists(_.chat.`type` == ChatType.Private) =>
      authenticate(query.from.username, query.message.map(_.chat.id)).flatMap {
        case Some((state, user)) =>
          processCallbackQuery(user)(state, query)
        case _ =>
          Monad[F].unit
      }
    case _ =>
      Monad[F].unit

  }

  private[this] def authenticateAndProcessMessage(message: Message): F[Unit] =
    authenticate(message.from.flatMap(_.username), Some(message.chat.id)).flatMap {
      case Some((state, user)) =>
        processMessage(user)(state, message)
      case _ =>
        sender.tell(message.chat.id, "Пользователь не аутентифицирован для данного сервиса")
    }


  private[this] def authenticate(username: Option[String], chatId: Option[Long]): F[Option[(ChatState, PersistedUser)]] = (for (
    name   <- OptionT.fromOption[F](username);
    user   <- OptionT(usersRepository(name));
    id     <- OptionT.fromOption[F](chatId);
    _      <- OptionT.liftF(usersRepository.setChatIdIfNeeded(user, id));
    state  <- OptionT.liftF(chatStateRepository.get(id))
  ) yield (state, user)).value

  private[this] def processMessage(user: PersistedUser): (ChatState, Message) => F[Unit] =
    handlers(user)(
      HelpHandler(sender),
      VersionHandler(sender),
      OldShowHandler(user, notificationsRepository, sender),
      OldEditHandler(notificationsRepository, notificationChatService, sender),
      ListMessageHandler(user, pageView),
      InMessageHandler(user, systemDateTime, chatStateRepository, notificationChatService, notificationsRepository, sender)
    )

  private[this] def processCallbackQuery(user: PersistedUser): (ChatState, CallbackQuery) => F[Unit] =
    handlers(user)(
      ListCallbackHandler(user, pageView, notificationsRepository, notificationChatService, chatStateRepository, sender)
    )

  private[this] def handlers[T](user: PersistedUser)(functions: BotHandler[T, F]*)(chatState: ChatState, message: T) =
    functions.reduce(_ orElse _).applyOrElse[(ChatState, T), F[Unit]](chatState -> message,
      pair => UnknownHandler[T, F](user, sender)(pair._1, pair._2))

}

