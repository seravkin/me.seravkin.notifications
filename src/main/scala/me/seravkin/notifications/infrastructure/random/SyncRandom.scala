package me.seravkin.notifications.infrastructure.random

import cats._
import cats.effect.Sync
import cats.implicits._

import scala.util

final class SyncRandom[F[_]: Sync] extends Random[F] {
  override def nextInt(int: Int): F[Int] =
    Sync[F].delay(new util.Random().nextInt(int))
}
