package me.seravkin.notifications.infrastructure.interpreters

import cats.effect.IO
import cats.{Id, ~>}

object UnsafeIOInterpreter extends (IO ~> Id) {
  override def apply[A](fa: IO[A]): Id[A] =
    fa.unsafeRunSync()
}
