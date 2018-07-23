package me.seravkin.notifications.bot.commands

object ChangeNotificationText {
  private[this] val HasNotificationId = "notification-date-([0-9]+)-([0-9]+)-(.*)".r

  def apply(msgId: Int,  id: Long, commandToReturn: String): String = s"notification-date-$msgId-$id-$commandToReturn"

  def unapply(arg: String): Option[(Int, Long, String)] = HasNotificationId
    .unapplySeq(arg)
    .collect {
      case msgId :: long :: command :: Nil => (msgId.toInt, long.toLong, command)
    }
}
