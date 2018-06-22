package me.seravkin.notifications.bot.commands

object SelectNotificationDate {
  private val HasSelectNotificationDate = "select-date-([0-9]+)-([0-9]+)".r

  def apply(day: Int, month: Int): String =
    s"select-date-$day-$month"

  def unapply(arg: String): Option[(Int, Int)] = HasSelectNotificationDate.unapplySeq(arg)
    .collect {
      case day :: month :: Nil => (day.toInt, month.toInt)
    }
}
