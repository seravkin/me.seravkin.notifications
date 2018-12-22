package me.seravkin.notifications.domain.interpreter

import java.time.LocalDateTime

import cats._
import me.seravkin.notifications.domain.ast.DateAst

final class DateApplicativeAst[F[_]: Applicative: DateProvider] extends DateAst[F[Dates]] {
  override def date(day: Int, month: Int, year: Int): F[Dates] = ApplicativeAst.factoryOf[F] { _ =>
    LocalDateTime.of(year, month, day, 0, 0)
  }

  override def date(day: Int, month: Int): F[Dates] = ApplicativeAst.factoryOf[F] { now =>
    LocalDateTime.of(now.getYear, month, day, 0, 0)
  }
}
