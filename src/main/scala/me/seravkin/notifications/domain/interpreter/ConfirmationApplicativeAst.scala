package me.seravkin.notifications.domain.interpreter

import java.time.Duration

import cats._
import cats.implicits._
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.domain.ast._

final class ConfirmationApplicativeAst[F[_]: DateProvider: MonadError[?[_], String]] extends ConfirmationAst[F[Dates]] {
  override def confirmation(duration: Option[Duration], t: F[Dates]): F[Dates] =
    t.flatMap {
      case OneDate(dt) =>
        Monad[F].pure(Confirmation(dt, duration.getOrElse(Duration.ofMinutes(5))))
      case value =>
        ApplicativeError[F, String].raiseError[Dates](s"Ошибка типа для $value")
    }
}
