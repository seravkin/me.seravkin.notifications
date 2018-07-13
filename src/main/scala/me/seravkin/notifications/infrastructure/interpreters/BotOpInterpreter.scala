package me.seravkin.notifications.infrastructure.interpreters

import cats._
import cats.implicits._
import cats.data._
import cats.effect._
import cats.instances.future._
import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor
import info.mukel.telegrambot4s.api.RequestHandler
import info.mukel.telegrambot4s.methods.{EditMessageText, SendMessage}
import info.mukel.telegrambot4s.models.{InlineKeyboardButton, InlineKeyboardMarkup, ReplyKeyboardMarkup}
import me.seravkin.notifications.bot.NotificationBot.{ChatState, Nop}
import me.seravkin.notifications.domain.algebra.BotAlgebra._
import me.seravkin.notifications.infrastructure.messages.Button
import me.seravkin.tg.adapter.requests.RequestHandlerF
import monix.eval.Task


import scala.collection.concurrent.TrieMap

final case class BotOpInterpreter(map: TrieMap[Long, ChatState], requestHandler: RequestHandlerF[IO])
  extends (BotOp ~> ReaderT[IO, Transactor[IO], ?]) {

  private[this] def buttonsToMarkup(buttons: List[Button]): Option[InlineKeyboardMarkup] = Some(InlineKeyboardMarkup.singleColumn(
    buttons.map(b =>
      InlineKeyboardButton.callbackData(b.name, b.command))
  ))


  override def apply[A](fa: BotOp[A]): ReaderT[IO, Transactor[IO], A] = ReaderT[IO, Transactor[IO], A](xa => fa match {
    case Get(chatId) => IO {
      map.getOrElse(chatId, Nop)
    }

    case Set(chatId, state) => IO {
      map += chatId -> state
      ()
    }
    case Send(chatId, text, buttons, None) => requestHandler
      .ask(SendMessage(chatId, text, replyMarkup = buttonsToMarkup(buttons)))
      .map(_.messageId)

    case Send(chatId, text, buttons, Some(msgId)) => requestHandler
      .ask(EditMessageText(Some(chatId), Some(msgId), text = text, replyMarkup = buttonsToMarkup(buttons)))
      .map(_ => msgId)

    case DatabaseAction(action) =>
      xa.rawTrans.apply(action)
  })
}
