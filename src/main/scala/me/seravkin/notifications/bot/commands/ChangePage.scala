package me.seravkin.notifications.bot.commands

object ChangePage {

  private[this] val HasPage = "page-([0-9]+)-([0-9]+)-([0-9]+)".r

  def apply(messageId: Int, skip: Int, take: Int): String = s"page-$messageId-$skip-$take"

  def unapply(arg: String): Option[(Int, Int, Int)] = HasPage.unapplySeq(arg)
    .collect {
      case id :: skip :: take :: Nil => (id.toInt, skip.toInt, take.toInt)
    }

}

