package me.seravkin.notifications.infrastructure.telegram.requests

import com.bot4s.telegram.methods.Request

trait RequestHandlerF[F[_]] {
  /**
   * Send request and ignore result
   */
  def tell[R: Manifest](apiRequest: Request[R]): F[Unit]

  /**
   * Send request and get result
   */
  def ask[R: Manifest](apiRequest: Request[R]): F[R]

  /**
   * Send request and ignore result
   */
  def apply[R: Manifest](apiRequest: Request[R]): F[Unit] = tell(apiRequest)
}
