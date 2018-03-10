package me.seravkin.notifications.integration

import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.infrastructure
import me.seravkin.notifications.infrastructure.messages

object TelegramForScalaIntegration {

  implicit def messageForMessage: infrastructure.messages.Message[Message] = new messages.Message[Message] {
    override def data(msg: Message): Option[String] =
      None

    override def chatId(msg: Message): Long =
      msg.chat.id

    override def text(msg: Message): Option[String] =
      msg.text

    override def username(msg: Message): Option[String] =
      msg.chat.username

    override def isPrivate(msg: Message): Boolean =
      msg.chat.`type` == ChatType.Private
  }

  implicit def messageForCallbackQuery: infrastructure.messages.Message[CallbackQuery] = new messages.Message[CallbackQuery] {
    override def data(msg: CallbackQuery): Option[String] =
      msg.data

    override def chatId(msg: CallbackQuery): Long =
      msg.message.get.chat.id

    override def text(msg: CallbackQuery): Option[String] =
      None

    override def username(msg: CallbackQuery): Option[String] =
      msg.from.username

    override def isPrivate(msg: CallbackQuery): Boolean =
      msg.message.get.chat.`type` == ChatType.Private
  }

}
