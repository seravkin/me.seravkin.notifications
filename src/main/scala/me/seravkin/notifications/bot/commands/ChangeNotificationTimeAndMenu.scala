package me.seravkin.notifications.bot.commands

object ChangeNotificationTimeAndMenu {
  private[this] val HasNotificationId = "notification-([0-9]+)-([0-9]+)-(.*)".r

  def apply(msgId: Int,  id: Long, commandToReturn: String): String = s"notification-$msgId-$id-$commandToReturn"

  def unapply(arg: String): Option[(Int, Long, String)] = HasNotificationId
    .unapplySeq(arg)
    .collect {
      case msgId :: long :: command :: Nil => (msgId.toInt, long.toLong, command)
    }
}
