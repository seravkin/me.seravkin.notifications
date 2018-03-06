package me.seravkin.notifications.infrastructure.interpreters

import cats._
import cats.instances.future._
import doobie._
import doobie.implicits._
import doobie.util.transactor.Transactor
import info.mukel.telegrambot4s.api.RequestHandler
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models.{InlineKeyboardButton, InlineKeyboardMarkup, ReplyKeyboardMarkup}
import me.seravkin.notifications.bot.NotificationBot.{ChatState, Nop}
import me.seravkin.notifications.domain.algebra.BotAlgebra._
import monix.eval.Task

import scala.collection.concurrent.TrieMap

class BotIOInterpreter(chatId: Option[Long], map: TrieMap[Long, ChatState], requestHandler: RequestHandler, transactor: Transactor[Task]) extends (BotOp ~> Task) {

  override def apply[A](fa: BotOp[A]): Task[A] = fa match {
    case Get() => Task {
      map.getOrElse(chatId.getOrElse(-1), Nop).asInstanceOf[A]
    }

    case Set(state) =>
      for(chatIdClear <- chatId) {
        map += chatIdClear -> state.asInstanceOf[ChatState]
      }
      Task { () }

    case Send(chatId, text, buttons) => Task.fromFuture {
        requestHandler.apply(SendMessage(chatId, text, replyMarkup = Some(InlineKeyboardMarkup.singleColumn(
          buttons.map(b =>
            InlineKeyboardButton.callbackData(b.name, b.command))
        ))))
      }.map(_ => ())

    case DatabaseAction(action) =>
      transactor.rawTrans.apply(action)
  }
}
