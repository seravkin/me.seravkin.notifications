package me.seravkin.notifications.domain.interpreter

import java.time.LocalDateTime

import cats.{ApplicativeError, Monad, MonadError}
import cats.mtl.ApplicativeAsk
import cats.syntax.flatMap._
import cats.syntax.functor._
import me.seravkin.notifications.domain.ast.RecurrentAst
import me.seravkin.notifications.domain.interpreter.Dates.{Periodic, RecurrencyType}

final class RecurrentApplicativeAst[F[_]: MonadError[*[_], String]
                                        : DateProvider] extends RecurrentAst[F[Dates]] {

  override def everyDayOfMonth(days: Set[Int], t: F[Dates]): F[Dates] =
    Monad[F].map(t) {
      case p: Periodic => p.copy(days = days, recurrencyType = RecurrencyType.Month)
      case other => other
    }

  override def everyDayOfWeek(days: Set[Int], recurrent: F[Dates]): F[Dates] =
    Monad[F].map(recurrent) {
      case p: Periodic => p.copy(days = days)
      case other => other
    }

  override def before(day: Int, month: Int, year: Option[Int], recurrent: F[Dates]): F[Dates] =
    for(now <- ApplicativeAsk[F, LocalDateTime].ask;
        rec <- recurrent)
      yield rec match {
        case p: Periodic => p.copy(end = Some(LocalDateTime.of(year.getOrElse(now.getYear), month, day, 0, 0)))
        case other => other
      }

  override def after(day: Int, month: Int, year: Option[Int], recurrent: F[Dates]): F[Dates] =
    for(now <- ApplicativeAsk[F, LocalDateTime].ask;
        rec <- recurrent)
      yield rec match {
        case p: Periodic => p.copy(start = Some(LocalDateTime.of(year.getOrElse(now.getYear), month, day, 0, 0)))
        case other => other
      }

  override def inTime(hours: Int, minutes: Int): F[Dates] = ApplicativeAsk[F, LocalDateTime].ask.flatMap { now =>
    Periodic(hours, minutes, (0 until 7).toSet, RecurrencyType.Week, None, None)(now) match {
      case Some(periodic) => Monad[F].pure(periodic)
      case None => ApplicativeError[F, String]
        .raiseError[Dates](s"Напоминание не будет запущено т.к. $now позже даты конца напоминаний")
    }
  }

}
