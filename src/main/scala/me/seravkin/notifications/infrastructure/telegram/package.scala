package me.seravkin.notifications.infrastructure

import cats.data.Kleisli
import me.seravkin.notifications.infrastructure.telegram.events.BotEvent

package object telegram {
  type Bot[F[_]] = Kleisli[F, BotEvent, Unit]
}
