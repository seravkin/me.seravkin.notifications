package me.seravkin.notifications.test.mocks

import java.time.LocalDateTime

import me.seravkin.notifications.bot.ChatState
import me.seravkin.notifications.bot.ChatState._
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import shapeless._

final case class MockBotState(users: List[PersistedUser] = Nil,
                              notifications: List[Notification] = Nil,
                              sentMessages: List[MockMessage] = Nil,
                              chatState: ChatState = Nop,
                              compiledDatesIdToDate: Map[Long, LocalDateTime] = Map.empty)

object MockBotState {

  val messages = lens[MockBotState] >> 'sentMessages
  val state = lens[MockBotState] >> 'chatState
  val notifications = lens[MockBotState] >> 'notifications
  val compiled = lens[MockBotState] >> 'compiledDatesIdToDate

}