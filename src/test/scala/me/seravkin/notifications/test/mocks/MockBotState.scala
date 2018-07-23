package me.seravkin.notifications.test.mocks

import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.bot.{ChatState, Nop}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.tg.adapter.events.BotEvent
import shapeless._

final case class MockBotState(users: List[PersistedUser] = Nil,
                              notifications: List[Notification] = Nil,
                              sentMessages: List[MockMessage] = Nil,
                              chatState: ChatState = Nop)

object MockBotState {

  val messages = lens[MockBotState] >> 'sentMessages
  val state = lens[MockBotState] >> 'chatState
  val notifications = lens[MockBotState] >> 'notifications

}