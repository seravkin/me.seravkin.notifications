package me.seravkin.notifications.test.mocks

import me.seravkin.notifications.bot.NotificationBot.{ChatState, Nop}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.User
import shapeless._

final case class MockBotState(users: List[User] = Nil,
                              notifications: List[Notification] = Nil,
                              sentMessages: List[MockMessage] = Nil,
                              chatState: ChatState = Nop)

object MockBotState {

  val messages = lens[MockBotState] >> 'sentMessages
  val state = lens[MockBotState] >> 'chatState
  val notifications = lens[MockBotState] >> 'notifications

}