package me.seravkin.notifications.bot

import cats._
import cats.implicits._
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.tg.adapter.matching._

object HelpHandler {

  private[this] val HELP_TEXT = "Бот c напоминаниями\n" +
    "/in - Напоминает о событии через заданный интервал времени\n" +
    "/show - Показывает активные напоминания\n" +
    "/delete <id> - Удаляет напоминания с указанным id\n" +
    "/change <id> - Изменяет дату и время на напоминании с указанным id\n" +
    "/list - Показывает новый список напоминаний"


  def apply[F[_]: Monad](sender: Sender[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText("/help")) =>
      sender.ask(message.chat.id, HELP_TEXT) >>
      Monad[F].unit

    case HasMessage(message@ContainsText("/start")) =>
      sender.ask(message.chat.id, HELP_TEXT) >>
      Monad[F].unit
  }
}