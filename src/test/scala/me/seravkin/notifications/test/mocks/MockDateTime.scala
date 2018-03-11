package me.seravkin.notifications.test.mocks

import java.time.LocalDateTime

import me.seravkin.notifications.infrastructure.time.SystemDateTime

final case class MockDateTime(dateTime: LocalDateTime) extends SystemDateTime {
  override def now: LocalDateTime = dateTime
}
