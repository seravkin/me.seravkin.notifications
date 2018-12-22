package me.seravkin.notifications

import cats.Eval
import cats.data.StateT
import me.seravkin.notifications.test.mocks.MockBotState

package object test {
  type MockBotF[A] = StateT[Eval, MockBotState, A]
}
