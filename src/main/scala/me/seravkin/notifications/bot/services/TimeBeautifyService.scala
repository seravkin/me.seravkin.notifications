package me.seravkin.notifications.bot.services

import me.seravkin.notifications.domain.interpreter.Dates

trait TimeBeautifyService {
  def beautify(dates: Dates): String
}
