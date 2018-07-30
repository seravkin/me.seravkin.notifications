package me.seravkin.notifications.domain

import java.time.LocalDateTime

import cats.data.ReaderT

package object interpreter {
  final case class NotificationPrototype[F[_]](userName: Option[String],
                                               isRelative: Boolean,
                                               reader: ReaderT[F, LocalDateTime, LocalDateTime]) {
    def apply(localDateTime: LocalDateTime): F[LocalDateTime] = reader(localDateTime)
  }
}
