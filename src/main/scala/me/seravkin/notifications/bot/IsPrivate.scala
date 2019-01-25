package me.seravkin.notifications.bot

import com.bot4s.telegram.models.{ChatType, Message}
import me.seravkin.notifications.infrastructure.messages.Sender

object IsPrivate {
  def apply[F[_]](sender: Sender[F])(f: Message => F[Unit])(message: Message): F[Unit] =
    if(message.chat.`type` == ChatType.Private)
      f(message)
    else
      sender.tell(message.chat.id, "Бот поддерживает только приватные беседы ")
}
