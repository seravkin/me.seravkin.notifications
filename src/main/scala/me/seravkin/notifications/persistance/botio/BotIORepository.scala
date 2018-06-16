package me.seravkin.notifications.persistance.botio

import cats.free.Free
import doobie.free.connection.ConnectionIO
import doobie.util.composite.Composite
import doobie.util.fragment.Fragment
import fs2.Stream
import me.seravkin.notifications.domain.algebra.BotAlgebra.{BotIO, DatabaseAction}

trait BotIORepository {
  def botIO[A](connectionIO: => ConnectionIO[A]): BotIO[A] =
    Free.liftF(DatabaseAction(connectionIO))

  implicit class FragmentOps(fragment: Fragment) {
    def read[Entity: Composite]: Stream.ToEffect[doobie.ConnectionIO, Entity] = fragment
      .query[Entity]
      .stream
      .compile
  }
}
