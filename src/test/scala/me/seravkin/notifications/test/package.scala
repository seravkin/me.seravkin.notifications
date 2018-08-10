package me.seravkin.notifications

import cats.data.StateT
import cats.effect.IO
import me.seravkin.notifications.test.mocks.MockBotState

package object test {
  type MockBotF[A] = StateT[IO, MockBotState, A]
}
