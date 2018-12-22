package me.seravkin.notifications.domain.interpreter

import java.time.LocalDateTime

import cats._
import cats.implicits._
import cats.mtl.ApplicativeAsk
import me.seravkin.notifications.domain.interpreter.Dates.OneDate

object ApplicativeAst {
  def factoryOf[F[_]: Applicative: DateProvider](f: LocalDateTime => LocalDateTime): F[Dates] =
    ApplicativeAsk[F, LocalDateTime].ask.map(n => OneDate(f(n)))
}
