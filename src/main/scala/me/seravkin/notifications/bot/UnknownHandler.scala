package me.seravkin.notifications.bot

import cats._
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.Sender

object UnknownHandler {
  def apply[T, F[_]: Monad](user: PersistedUser, sender: Sender[F])(chatState: ChatState, msg: T): F[Unit] =
    user.chatId.map(sender.tell(_, "Неизвестная команда")).getOrElse(Monad[F].unit)
}
