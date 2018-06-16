package me.seravkin.notifications.bot.commands

object DeleteNotification {

  private[this] val HasDeleteNotification = "delete-([0-9]+)-([0-9]+)-(.*)".r

  def apply(messageId: Int, notificationId: Long, commandToReturn: String): String =
    s"delete-$messageId-$notificationId-$commandToReturn"

  def unapply(arg: String): Option[(Int, Long, String)] = HasDeleteNotification.unapplySeq(arg)
    .collect {
      case id :: notificationId :: returnCommand :: Nil => (id.toInt, notificationId.toLong, returnCommand)
    }

}
