package me.seravkin.notifications.persistance.botio

import cats._
import cats.data._
import cats.effect.Bracket
import doobie._
import doobie.implicits.toConnectionIOOps
import fs2.Stream

trait BotIORepository[F[_]] {
  def botIO[A](connectionIO: => ConnectionIO[A])(implicit evM: Monad[F], bracket: Bracket[F, Throwable]): ReaderT[F, Transactor[F], A] = ReaderT { tx =>
    connectionIO.transact(tx)
  }

  implicit class FragmentOps(fragment: Fragment) {
    def read[Entity: Read]: Stream.CompileOps[doobie.ConnectionIO, doobie.ConnectionIO, Entity] = fragment
      .query[Entity]
      .stream
      .compile
  }
}
