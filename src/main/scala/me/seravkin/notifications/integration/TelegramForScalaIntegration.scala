package me.seravkin.notifications.integration

import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.infrastructure
import me.seravkin.notifications.infrastructure.messages
import me.seravkin.tg.adapter.events._

object TelegramForScalaIntegration {

  implicit def messageForEvent: infrastructure.messages.Message[BotEvent] = new messages.Message[BotEvent] {
    override def username(msg: BotEvent): Option[String] = msg match {
      case ReceiveMessage(message) => message.chat.username
      case ReceiveEditedMessage(message) => message.chat.username
      case ReceiveCallbackQuery(query) => query.from.username
      case _ => None
    }

    override def text(msg: BotEvent): Option[String] = msg match {
      case ReceiveMessage(message) => message.text
      case ReceiveEditedMessage(message) => message.text
      case _ => None
    }

    override def chatId(msg: BotEvent): Long = msg match {
      case ReceiveMessage(message) => message.chat.id
      case ReceiveEditedMessage(message) => message.chat.id
      case ReceiveCallbackQuery(query) => query.from.id
    }

    override def isPrivate(msg: BotEvent): Boolean = msg match {
      case ReceiveMessage(message) => message.chat.`type` == ChatType.Private
      case ReceiveEditedMessage(message) => message.chat.`type` == ChatType.Private
      case ReceiveCallbackQuery(query) => query.message.get.chat.`type` == ChatType.Private
      case _ => false
    }

    override def data(msg: BotEvent): Option[String] = msg match {
      case ReceiveCallbackQuery(query) => query.data
      case _ => None
    }
  }

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
