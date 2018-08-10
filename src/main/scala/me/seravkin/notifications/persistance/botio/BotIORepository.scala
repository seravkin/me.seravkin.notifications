package me.seravkin.notifications.persistance.botio

import cats._
import cats.implicits._
import cats.effect._
import cats.data._
import doobie._
import doobie.implicits.{toConnectionIOOps, _}
import fs2.Stream

trait BotIORepository[F[_]] {
  def botIO[A](connectionIO: => ConnectionIO[A])(implicit evM: Monad[F]): ReaderT[F, Transactor[F], A] = ReaderT { tx =>
    connectionIO.transact(tx)
  }

  implicit class FragmentOps(fragment: Fragment) {
    def read[Entity: Composite]: Stream.ToEffect[doobie.ConnectionIO, Entity] = fragment
      .query[Entity]
      .stream
      .compile
  }
}
