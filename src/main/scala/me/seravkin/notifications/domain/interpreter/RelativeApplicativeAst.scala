package me.seravkin.notifications.domain.interpreter

import cats._
import me.seravkin.notifications.domain.ast.RelativeAst

final class RelativeApplicativeAst[F[_]: Applicative: DateProvider] extends RelativeAst[F[Dates]] {
  override def dayOfWeek(weekOffset: Int, dayOfWeek: Int): F[Dates] = ApplicativeAst.factoryOf[F] { now =>
    val isAfterGivenDayOfWeek = now.getDayOfWeek.ordinal() >= dayOfWeek
    val trueWeekOffset  = if(isAfterGivenDayOfWeek) weekOffset + 1 else weekOffset

    now.minusDays(now.getDayOfWeek.ordinal()).plusDays(7 * trueWeekOffset).plusDays(dayOfWeek)
  }

  override def inDays(days: Int): F[Dates] = ApplicativeAst.factoryOf[F] { now =>
    now.withHour(0).withMinute(0).plusDays(days)
  }
}
