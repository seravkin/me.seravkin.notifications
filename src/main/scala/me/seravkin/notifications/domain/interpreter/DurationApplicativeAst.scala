package me.seravkin.notifications.domain.interpreter

import java.time.Duration

import cats._
import me.seravkin.notifications.domain.ast.DurationAst

final class DurationApplicativeAst[F[_]: Applicative: DateProvider] extends DurationAst[F[Dates]] {
  override def duration(duration: Duration): F[Dates] = ApplicativeAst.factoryOf[F] { now =>
    now.plus(duration)
  }
}
