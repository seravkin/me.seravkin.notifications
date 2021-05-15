package me.seravkin.notifications.infrastructure.telegram

import com.bot4s.telegram.models.{CallbackQuery, Message}

object matching {
  object CommandWithArgs {
    def unapply(arg: String): Option[(String, List[String])] = arg match {
      case s if s.isEmpty => None
      case s =>
        val args = s.split(" ").toList
        Some(args.head -> args.tail)
    }
  }

  /** Unapplier for commands with quoted or non-quoted arguments
   * Example: /in "test one" testTwo "test Three"
   */
  object CommandWithQuotedArgs {
    private[this] val Arguments = """"(.+)"|([^\s]+)""".r

    def unapply(arg: String): Option[(String, List[String])] = arg match {
      case s if s.isEmpty => None
      case s =>
        Arguments.unapplySeq(s).map(l => l.head -> l.tail)
    }
  }

  /**
   * Unapplier for working with last arguments of command as text
   */
  object TailAsText {
    def unapply(arg: List[String]): Option[String] =
      if(arg.isEmpty)
        None
      else
        Some(arg.reduce(_ + " " + _))
  }

  /**
   * Unapplier for data in callback query
   */
  object ContainsData {
    def unapply(event: CallbackQuery): Option[String] =
      event.data
  }

  /**
   * Unapplier for text in message
   */
  object ContainsText {
    def unapply(event: Message): Option[String] =
      event.text
  }
}
