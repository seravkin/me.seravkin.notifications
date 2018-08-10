package me.seravkin.notifications

import cats._
import cats.implicits._
import info.mukel.telegrambot4s.models.CallbackQuery
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.interpreter._

package object bot {

  sealed trait ChatState

  final case object Nop extends ChatState
  final case object InControlWaitingForText extends ChatState
  final case class InControlWaitingForTextEdit(notificationId: Long) extends ChatState
  final case class InControlWaitingForTime(chatId: Long, text: String) extends ChatState
  final case class InControlWaitingForConfirmation(chatId: Long, text: String, time: String) extends ChatState

  type BotHandler[Msg, F[_]] = PartialFunction[(ChatState, Msg), F[Unit]]

  def hasChatId[F[_]](f: Long => BotHandler[CallbackQuery, F]): BotHandler[CallbackQuery, F] = new BotHandler[CallbackQuery, F] {
    override def isDefinedAt(x: (ChatState, CallbackQuery)): Boolean =
      x._2.message.nonEmpty && f(x._2.message.map(_.chat.id).get).isDefinedAt(x)

    override def apply(v1: (ChatState, CallbackQuery)): F[Unit] =
      f(v1._2.message.map(_.chat.id).get).apply(v1)
  }

  object HasMessage {
    def unapply[Msg](arg: (ChatState, Msg)): Option[Msg] = arg match {
      case (Nop, msg) => Some(msg)
      case _ => None
    }
  }

  object HasState {
    def unapply[Msg](arg: (ChatState, Msg)): Option[(ChatState, Msg)] =
      Some(arg)
  }

  //TODO: Правильное отображение для других типов
  //TODO: выразить через Show[T]
  implicit val showForListOfNotifications: Show[List[Notification]] = Show.show { notifications =>
    "Напоминания:\n" +
      notifications.collect {
        case Notification(id, _, text, _, n: OneDate) =>
          s"Напоминание $id о " + "\"" + text + "\" в " + n.notificationDate.toString
        case Notification(id, _, text, _, _) => s"Напоминание $id о " + "\"" + text + "\""
      }.foldLeft("") {
        _ + "\n" + _
      }
  }

}
