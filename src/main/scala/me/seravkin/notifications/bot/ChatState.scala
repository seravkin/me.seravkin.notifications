package me.seravkin.notifications.bot

sealed trait ChatState

object ChatState {

  final case object Nop extends ChatState
  final case object InControlWaitingForText extends ChatState
  final case class InControlWaitingForTextEdit(notificationId: Long) extends ChatState
  final case class InControlWaitingForTime(chatId: Long, text: String) extends ChatState
  final case class InControlWaitingForConfirmation(chatId: Long, text: String, time: String) extends ChatState

}
