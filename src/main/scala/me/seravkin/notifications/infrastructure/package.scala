package me.seravkin.notifications

import cats.data.ReaderT
import doobie.util.transactor.Transactor

package object infrastructure {
  type BotF[F[_], A] = ReaderT[F, Transactor[F], A]
}
