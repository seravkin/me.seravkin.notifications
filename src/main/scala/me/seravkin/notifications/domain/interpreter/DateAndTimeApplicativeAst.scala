package me.seravkin.notifications.domain.interpreter

import cats.{ApplicativeError, Monad, MonadError}
import cats.syntax.flatMap._
import cats.syntax.functor._
import me.seravkin.notifications.domain.ast.DateAndTimeAst
import me.seravkin.notifications.domain.interpreter.Dates.OneDate

final class DateAndTimeApplicativeAst[F[_]: DateProvider
                                          : MonadError[*[_], String]]
  extends DateAndTimeAst[F[Dates], F[Dates], F[Dates]] {

  private[this] def resultOrError(date: Dates, time: Dates): F[Dates] = (date, time) match {
    case (OneDate(dt), OneDate(tm)) =>
      Monad[F].pure(OneDate(dt.withHour(tm.getHour).withMinute(tm.getMinute)))
    case _ =>
      ApplicativeError[F, String].raiseError[Dates](s"Ошибка типов для $date и $time")
  }

  override def dateAndTime(date: F[Dates], time: F[Dates]): F[Dates] = for(
    dt <- date;
    tm <- time;
    re <- resultOrError(dt, tm)
  ) yield re
}
