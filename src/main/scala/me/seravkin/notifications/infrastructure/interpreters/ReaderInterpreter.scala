package me.seravkin.notifications.infrastructure.interpreters

import java.sql.Connection

import cats._
import cats.implicits._
import cats.data._
import cats.effect.ExitCase.Completed
import cats.effect._
import doobie.free.KleisliInterpreter
import doobie.free.connection.unit
import doobie.util.transactor.{Strategy, Transactor}

final class ReaderInterpreter(xa: => Connection) extends (ReaderT[IO, Transactor[IO], ?] ~> IO) {
  override def apply[A](fa: ReaderT[IO, Transactor[IO], A]): IO[A] =
    IO(xa).bracketCase { connection =>

      connection.setAutoCommit(false)

      val transactor = Transactor[IO, Connection](
        connection,
        IO.pure[Connection],
        KleisliInterpreter[IO].ConnectionInterpreter,
        Strategy.default.copy(always = unit, after = unit, oops = unit))

      fa(transactor)

    } {
      case (connection, Completed) => IO {
        connection.commit()
        connection.close()
      }
      case (connection, _) => IO {
        connection.rollback()
        connection.close()
      }
    }

}
