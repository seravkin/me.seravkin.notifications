package me.seravkin.notifications.bot.services
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

import me.seravkin.notifications.domain.interpreter
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.infrastructure.time.SystemDateTime

final class TimeBeautifyServiceImpl(systemDateTime: SystemDateTime) extends TimeBeautifyService {

  private[this] def beautify(time: LocalDateTime) = {
    val formattedTime = time.format(DateTimeFormatter.ofPattern(TIME_FORMAT))

    val date =
      if (systemDateTime.now.getDayOfYear != time.getDayOfYear)
        time.format(DateTimeFormatter.ofPattern(DATE_SHORT))
      else
        ""

    val dateWhiteSpace = if (date.isEmpty) "" else date + " "

    dateWhiteSpace + "в " + formattedTime
  }

  private[this] val DATE_SHORT = "dd.MM"
  private[this] val TIME_FORMAT = "HH:mm"

  override def beautify(dates: interpreter.Dates): String = dates match {
    case OneDate(ldt) =>
      beautify(ldt)
    case Confirmation(localDateTime, period) =>
      s"${beautify(localDateTime)} с подтверждением каждые ${period.toString}"
    case Periodic(localDateTime, _, _, days, _, _) =>
      s"${beautify(localDateTime)} с напоминаниями в следующие дни недели: ${days.map(_ + 1).map(_.toString).reduce { _ + ", " + _ }}"
  }
}
