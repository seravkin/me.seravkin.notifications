package me.seravkin.notifications.test.interpreter

import java.time.{Duration, LocalDateTime}

import me.seravkin.notifications.domain.interpreter._
import org.scalatest.{FlatSpec, Matchers}

class DatesSpec extends FlatSpec with Matchers {

  private[this] val previousNow = LocalDateTime.of(2018, 10, 10, 10, 10)
  private[this] val now = LocalDateTime.of(2018, 11, 11, 11, 11)
  private[this] val period = Duration.ofMinutes(30)


  "Dates" should "be constant for one time notification" in {
    OneDate(previousNow).next(now).notificationDate should be (previousNow)
  }

  it should "move in by period if with confirmation" in {
    Confirmation(previousNow, period).next(now).notificationDate should be (previousNow.plus(period))
    Confirmation(previousNow, period).next(now).next(now).notificationDate should be (previousNow.plus(period).plus(period))
  }

  it should "move to next week if periodic on one day" in {
    Periodic(12, 12, Set(1), None, None)(previousNow).notificationDate should be (LocalDateTime.of(2018, 10, 16, 12, 12))
  }

}
