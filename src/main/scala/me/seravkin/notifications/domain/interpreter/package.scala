package me.seravkin.notifications.domain

import java.time.LocalDateTime
import cats.data.ReaderT

package object interpreter {

  type DatesFactory[F[_]] = ReaderT[F, LocalDateTime, Either[String, Dates]]

}
