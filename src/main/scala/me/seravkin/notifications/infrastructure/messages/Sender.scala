package me.seravkin.notifications.infrastructure.messages

trait Sender[F[_]] {
  def send(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): F[Int]
}
