package me.seravkin.notifications.bot

import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.Locale

import cats._
import cats.data.OptionT
import cats.syntax.all._
import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.bot.NotificationBot._
import me.seravkin.notifications.domain._
import me.seravkin.notifications.domain.parsing.MomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.SystemDateTime
import me.seravkin.notifications.persistance.{NotificationsRepository, Page, UsersRepository}
import me.seravkin.notifications.bot.commands._
import me.seravkin.notifications.bot.services.{NotificationChatService, PageView}
import me.seravkin.notifications.domain.interpreter.DatesFactory
import me.seravkin.tg.adapter._
import me.seravkin.tg.adapter.events._

final case class NotificationBot[F[_] : Monad](usersRepository: UsersRepository[F],
                                               chatStateRepository: ChatStateRepository[ChatState, F],
                                               sender: Sender[F],
                                               momentInFutureParser: MomentInFutureParser[DatesFactory[F]],
                                               notificationsRepository: NotificationsRepository[F],
                                               notificationChatService: NotificationChatService[F],
                                               pageView: PageView[F],
                                               systemDateTime: SystemDateTime) extends (BotEvent => F[Unit]) {


  def apply(event: BotEvent): F[Unit] = event match {
    case ReceiveMessage(message) if message.chat.`type` == ChatType.Private =>
      authenticate(message.from.flatMap(_.username), Some(message.chat.id)).flatMap {
        case Some((state, user)) =>
          processMessage(user)(state, message)
        case _ =>
          sender.tell(message.chat.id, "Пользователь не аутентифицирован для данного сервиса")
      }
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
      OldShowHandler(user, notificationsRepository, sender),
      OldEditHandler(notificationsRepository, notificationChatService, sender),
      ListMessageHandler(user, pageView),
      InMessageHandler(user, chatStateRepository, notificationChatService, notificationsRepository, sender)
    )

  private[this] def processCallbackQuery(user: PersistedUser): (ChatState, CallbackQuery) => F[Unit] =
    handlers(user)(
      ListCallbackHandler(user, pageView, notificationsRepository, notificationChatService, chatStateRepository, sender),
      InCallbackHandler(user, notificationsRepository, sender, momentInFutureParser, notificationChatService, systemDateTime)
    )

  private[this] def handlers[T](user: PersistedUser)(functions: BotHandler[T, F]*)(chatState: ChatState, message: T) =
    functions.reduce(_ orElse _).applyOrElse[(ChatState, T), F[Unit]](chatState -> message,
      pair => UnknownHandler[T, F](user, sender)(pair._1, pair._2))

}

