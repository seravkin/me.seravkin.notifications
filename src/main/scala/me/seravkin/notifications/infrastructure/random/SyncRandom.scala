package me.seravkin.notifications.infrastructure.random

import cats.effect.Sync

final class SyncRandom[F[_]: Sync] extends Random[F] {
  override def nextInt(int: Int): F[Int] =
    Sync[F].delay(new util.Random().nextInt(int))
}
