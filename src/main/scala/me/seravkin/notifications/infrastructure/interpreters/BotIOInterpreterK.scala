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
import me.seravkin.notifications.bot.{ChatState, Nop}
import me.seravkin.notifications.domain.algebra.BotAlgebra._
import me.seravkin.notifications.infrastructure.messages.Button
import me.seravkin.tg.adapter.requests.RequestHandlerF
import monix.eval.Task

import scala.collection.concurrent.TrieMap

final case class BotIOInterpreterK[F[_]: Monad](k: BotOp ~> F) extends (BotIO ~> F) {
  override def apply[A](fa: BotIO[A]): F[A] =
    fa.foldMap(k)
}
