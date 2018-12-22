package me.seravkin.notifications.domain.interpreter

import java.time.LocalDateTime

import cats._
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.mtl.ApplicativeAsk
import me.seravkin.notifications.domain.ast.TimeAst
import me.seravkin.notifications.domain.interpreter.Dates.OneDate
import me.seravkin.notifications.domain.parsing.Period
import me.seravkin.notifications.infrastructure.random.Random

final class TimeApplicativeAst[F[_]: Monad: DateProvider](random: Random[F]) extends TimeAst[F[Dates]] {
  override def fuzzyTime(period: Period): F[Dates] = ApplicativeAsk.ask[F, LocalDateTime].flatMap { now =>
    val (start, end) = period.period

    random.nextInt(end - start + 1).map2(random.nextInt(8)) { case (deltaHour, fiveMin) =>
      OneDate(now.withHour(start + deltaHour).withMinute(fiveMin * 5))
    }
  }

  override def time(hours: Int, minutes: Int): F[Dates] = ApplicativeAst.factoryOf[F] { now =>
    now.withHour(hours).withMinute(minutes)
  }

  override def inCurrentTime: F[Dates] = ApplicativeAst.factoryOf[F] (identity)
}
