package me.seravkin.notifications

import cats.Id
import cats.data.StateT
import me.seravkin.notifications.test.mocks.MockBotState

package object test {
  type MockBotF[A] = StateT[Id, MockBotState, A]
}
