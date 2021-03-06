package me.seravkin.notifications.bot

import com.bot4s.telegram.models.Message
import me.seravkin.notifications.infrastructure.messages.Sender
import me.seravkin.notifications.infrastructure.telegram.matching.ContainsText

object HelpHandler {

  private[this] val HELP_TEXT = "Бот c напоминаниями\n" +
    "/in - Напоминает о событии через заданный интервал времени\n" +
    "/show - Показывает активные напоминания\n" +
    "/delete <id> - Удаляет напоминания с указанным id\n" +
    "/change <id> - Изменяет дату и время на напоминании с указанным id\n" +
    "/list - Показывает новый список напоминаний\n" +
    "/version - Показывает текущую версию"


  def apply[F[_]](sender: Sender[F]): BotHandler[Message, F] = {
    case HasMessage(message@ContainsText("/help")) =>
      sender.tell(message.chat.id, HELP_TEXT)

    case HasMessage(message@ContainsText("/start")) =>
      sender.tell(message.chat.id, HELP_TEXT)
  }
}
