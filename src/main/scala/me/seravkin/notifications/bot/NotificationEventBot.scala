package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import info.mukel.telegrambot4s.models.{CallbackQuery, Message}
import me.seravkin.tg.adapter.events._
import me.seravkin.notifications.integration.TelegramForScalaIntegration._

final case class NotificationEventBot[F[_]: Monad](messageBot: NotificationBot[Message, F],
                                                   queryBot: NotificationBot[CallbackQuery, F]) extends (BotEvent => F[Unit]){
  override def apply(event: BotEvent): F[Unit] = event match {
    case ReceiveMessage(message) =>
      messageBot(message)
    case ReceiveCallbackQuery(query) =>
      queryBot(query)
    case _ =>
      Monad[F].unit
  }
}
