package me.seravkin.notifications.bot

import cats._
import info.mukel.telegrambot4s.models.{ChatType, Message}
import me.seravkin.notifications.infrastructure.messages.Sender

object IsPrivate {
  def apply[F[_]: Monad](sender: Sender[F])(f: Message => F[Unit])(message: Message): F[Unit] =
    if(message.chat.`type` == ChatType.Private)
      f(message)
    else
      sender.tell(message.chat.id, "Бот поддерживает только приватные беседы ")
}
