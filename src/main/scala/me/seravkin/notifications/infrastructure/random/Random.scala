package me.seravkin.notifications.infrastructure.random

trait Random[F[_]] {
  def nextInt(int: Int): F[Int]
}
