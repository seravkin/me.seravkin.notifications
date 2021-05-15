package me.seravkin.notifications.infrastructure.telegram.requests

import cats.effect.{Concurrent, IO}
import cats.implicits.catsSyntaxFlatMapOps
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.methods.Request

final class RequestHandlerAdapter[F[_] : Concurrent](requestHandler: RequestHandler[IO]) extends RequestHandlerF[F] {

  /** @inheritdoc */
  override def ask[R: Manifest](apiRequest: Request[R]): F[R] =
    Concurrent[F].liftIO(requestHandler(apiRequest))

  /** @inheritdoc */
  override def tell[R: Manifest](apiRequest: Request[R]): F[Unit] =
    Concurrent[F].liftIO(requestHandler(apiRequest) >> IO.unit)

}
