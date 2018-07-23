package me.seravkin.notifications.test.mocks

import me.seravkin.notifications.domain.{PersistedUser}
import me.seravkin.notifications.infrastructure.messages.{Button, Message}

final case class MockMessage(id: Int, user: PersistedUser, text: String, data: Option[String] = None, buttons: List[Button] = Nil)
