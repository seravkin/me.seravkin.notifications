package me.seravkin.notifications

import cats._
import com.bot4s.telegram.models.CallbackQuery
import me.seravkin.notifications.bot.ChatState.Nop
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.internationalization.LegacyInternationalization
import me.seravkin.notifications.domain.internationalization.Words._
import me.seravkin.notifications.domain.interpreter.Dates

package object bot {

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

  private val daysMap =
    List(DayOfWeek.Monday, DayOfWeek.Tuesday, DayOfWeek.Wednesday,
         DayOfWeek.Thursday, DayOfWeek.Friday, DayOfWeek.Saturday, DayOfWeek.Sunday)
    .map(d => (d.toInt, LegacyInternationalization.words(d).head))
    .toMap

  private def showDay(day: Int): String =
    daysMap(day)

  implicit val showForDates: Show[Dates] = Show.show {
    case Dates.OneDate(localDateTime) =>
      s" в $localDateTime"
    case Dates.Confirmation(localDateTime, period) =>
      s" в $localDateTime с подтверждением каждые $period"
    case Dates.Periodic(_, hour, minutes, days, start, end) =>
      s" в каждый из дней: ${days.map(showDay).reduce { _ + "," + _ }} в $hour:$minutes " +
      start.map(d => s"с началом в $d ").getOrElse("") +
      end.map(d => s"с окончанием в $d").getOrElse("")
  }

  //TODO: Правильное отображение для других типов
  //TODO: выразить через Show[T]
  implicit def showForListOfNotifications(implicit show: Show[Dates]): Show[List[Notification]] = Show.show { notifications =>
    "Напоминания:\n" +
      notifications.collect {
        case Notification(id, _, text, _, n) =>
          s"Напоминание $id о " + "\"" + text + "\"" + show.show(n)
      }.foldLeft("") {
        _ + "\n" + _
      }
  }

}
