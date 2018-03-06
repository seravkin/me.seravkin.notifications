package me.seravkin.notifications.infrastructure.scheduling

import scala.concurrent.duration.FiniteDuration

trait Scheduler[F[_]] {
  def schedule(initialDelay: FiniteDuration, interval: FiniteDuration)(action: => F[Unit]): Unit
}
