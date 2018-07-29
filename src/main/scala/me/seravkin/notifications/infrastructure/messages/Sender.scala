package me.seravkin.notifications.infrastructure.messages

trait Sender[F[_]] {
  def ask(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): F[Int]
  def tell(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): F[Unit]
}
