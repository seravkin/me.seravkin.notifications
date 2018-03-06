package me.seravkin.notifications.persistance.botio

import cats.free.Free
import doobie.free.connection.ConnectionIO
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, DatabaseAction}

trait BotIORepository {
  def botIO[A](connectionIO: => ConnectionIO[A]): BotIO[A] =
    Free.liftF(DatabaseAction(connectionIO))
}
