package me.seravkin.notifications.domain

import java.time.LocalDateTime

import cats.data.{EitherT, ReaderT}
import cats.mtl.ApplicativeAsk

package object interpreter {

  type DatesFactory[F[_], A] = ReaderT[EitherT[F, String,*], LocalDateTime, A]
  type DateProvider[F[_]] = ApplicativeAsk[F, LocalDateTime]

}
