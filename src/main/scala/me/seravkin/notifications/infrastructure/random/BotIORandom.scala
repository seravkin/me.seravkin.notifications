package me.seravkin.notifications.infrastructure.random

import cats._
import cats.implicits._
import cats.data._
import cats.free.Free
import me.seravkin.notifications.domain.algebra.BotAlgebra._

object BotIORandom extends Random[BotIO]{
  override def nextInt(int: Int): BotIO[Int] =
    Free.liftF(NextRandomInt(int))
}
