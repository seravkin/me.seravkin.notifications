package me.seravkin.notifications.bot.services

import me.seravkin.notifications.domain.interpreter.Dates

trait TimeBeautifyService[F[_]] {
  def beautify(dates: Dates): F[String]
}
