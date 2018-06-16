package me.seravkin.notifications.bot.commands

object ChangeNotificationTime {

  private[this] val HasNotificationId = "notification-([0-9]+)".r

  def apply(id: Long): String = s"notification-$id"

  def unapply(arg: String): Option[Long] = HasNotificationId
    .unapplySeq(arg)
    .collect {
      case long :: Nil => long.toLong
    }
}
