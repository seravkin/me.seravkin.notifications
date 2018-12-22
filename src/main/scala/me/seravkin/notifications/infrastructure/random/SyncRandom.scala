package me.seravkin.notifications.infrastructure.random

import cats.{Applicative, Defer}

final class SyncRandom[F[_]: Defer: Applicative] extends Random[F] {
  override def nextInt(int: Int): F[Int] =
    Defer[F].defer(Applicative[F].pure(new util.Random().nextInt(int)))
}
