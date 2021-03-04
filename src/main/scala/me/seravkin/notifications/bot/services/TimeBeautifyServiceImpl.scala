package me.seravkin.notifications.bot.services
import cats.Applicative
import cats.syntax.functor._

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import me.seravkin.notifications.domain.interpreter
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.infrastructure.time.SystemDateTime

final class TimeBeautifyServiceImpl[F[_]: Applicative](systemDateTime: SystemDateTime[F]) extends TimeBeautifyService[F] {

  private[this] def beautify(time: LocalDateTime) = systemDateTime.now.map { now =>
    val formattedTime = time.format(DateTimeFormatter.ofPattern(TIME_FORMAT))

    val date =
      if(now.getDayOfYear != time.getDayOfYear)
        time.format(DateTimeFormatter.ofPattern(DATE_SHORT))
      else
        ""

    val dateWhiteSpace = if (date.isEmpty) "" else date + " "

    dateWhiteSpace + "в " + formattedTime
  }

  private[this] val DATE_SHORT = "dd.MM"
  private[this] val TIME_FORMAT = "HH:mm"

  override def beautify(dates: interpreter.Dates): F[String] = dates match {
    case OneDate(ldt) =>
      beautify(ldt)
    case Confirmation(localDateTime, period) =>
      Applicative[F].pure(s"${beautify(localDateTime)} с подтверждением каждые ${period.toString}")
    case Periodic(localDateTime, _, _, days, period, _, _) =>
      val periodName = if(period == RecurrencyType.Month) "месяца" else "недели"
      Applicative[F].pure(s"${beautify(localDateTime)} с напоминаниями в следующие дни $periodName: ${days.map(_ + 1).map(_.toString).reduce { _ + ", " + _ }}")
  }
}
