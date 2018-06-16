package me.seravkin.notifications.bot.commands

object OpenNotificationMenu {

  private val HasOpenNotificationMenu = "open-menu-([0-9]+)-([0-9]+)-(.*)".r

  def apply(messageId: Int, notificationId: Long, commandToReturn: String): String =
    s"open-menu-$messageId-$notificationId-$commandToReturn"

  def unapply(arg: String): Option[(Int, Long, String)] = HasOpenNotificationMenu.unapplySeq(arg)
    .collect {
      case msgId :: notificationId :: commandToReturn :: Nil => (msgId.toInt, notificationId.toLong, commandToReturn)
    }
}
