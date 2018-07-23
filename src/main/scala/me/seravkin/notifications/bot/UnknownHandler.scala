package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.Sender

object UnknownHandler {
  def apply[T, F[_]: Monad](user: PersistedUser, sender: Sender[F])(chatState: ChatState, msg: T): F[Unit] =
    user.chatId.map(sender.ask(_, "Неизвестная команда") >> Monad[F].unit).getOrElse(Monad[F].unit)
}
