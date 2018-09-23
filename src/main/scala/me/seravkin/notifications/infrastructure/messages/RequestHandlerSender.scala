package me.seravkin.notifications.infrastructure.messages

import cats.effect.Sync
import cats.implicits._
import info.mukel.telegrambot4s.methods.{EditMessageText, SendMessage}
import info.mukel.telegrambot4s.models.{InlineKeyboardButton, InlineKeyboardMarkup}
import me.seravkin.tg.adapter.requests.RequestHandlerF

final class RequestHandlerSender[F[_]: Sync](requestHandlerF: RequestHandlerF[F]) extends Sender[F] {

  private[this] def buttonsToMarkup(buttons: List[Button]): Option[InlineKeyboardMarkup] = Some(InlineKeyboardMarkup.singleColumn(
    buttons.map(b =>
      InlineKeyboardButton.callbackData(b.name, b.command))
  ))

  override def ask(chatId: Long, text: String, buttonWithCommand: List[Button], idToEdit: Option[Int]): F[Int] = idToEdit match {
    case None => requestHandlerF
      .ask(SendMessage(chatId, text, replyMarkup = buttonsToMarkup(buttonWithCommand)))
      .map(_.messageId)

    case Some(msgId) =>
      requestHandlerF
        .ask(EditMessageText(Some(chatId), Some(msgId), text = text, replyMarkup = buttonsToMarkup(buttonWithCommand)))
        .map(_ => msgId)
  }

  override def tell(chatId: Long, text: String, buttonWithCommand: List[Button], idToEdit: Option[Int]): F[Unit] =
    ask(chatId, text, buttonWithCommand, idToEdit) >> Sync[F].unit

}
