package me.seravkin.notifications.test.interpreter

import java.time.{Duration, LocalDateTime}

import me.seravkin.notifications.domain.interpreter._
import org.scalatest.{FlatSpec, Matchers}

class DatesSpec extends FlatSpec with Matchers {

  private[this] val previousNow = LocalDateTime.of(2018, 10, 10, 10, 10)
  private[this] val now = LocalDateTime.of(2018, 11, 11, 11, 11)
  private[this] val period = Duration.ofMinutes(30)

  "Dates" should "be None for one time notification" in {
    OneDate(previousNow).next(now) should be (None)
  }

  it should "move in by period if with confirmation" in {
    Confirmation(previousNow, period).next(now).map(_.notificationDate) should be (Some(previousNow.plus(period)))
    Confirmation(previousNow, period).next(now).flatMap(_.next(now)).map(_.notificationDate) should be (Some(previousNow.plus(period).plus(period)))
  }

  it should "point on given day of week if periodic" in {
    Periodic(12, 12, Set(1), None, None)(previousNow).get.notificationDate should be (LocalDateTime.of(2018, 10, 16, 12, 12))
  }

  it should "move correctly on next days of week if periodic" in {
    val startAtWednesday = Periodic(12, 12, Set(0, 2, 4), None, None)(previousNow).get

    val toBeOnFriday = startAtWednesday.next(startAtWednesday.notificationDate)

    val toBeOnNextMonday = toBeOnFriday.flatMap(n => n.next(n.notificationDate))

    startAtWednesday.notificationDate should be (LocalDateTime.of(2018, 10, 10, 12, 12))
    toBeOnFriday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 12, 12, 12)))
    toBeOnNextMonday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 15, 12, 12)))
  }

  it should "respect given start date if periodic" in {
    val startOnFriday = Periodic(12, 12, Set(0, 2, 4), Some(LocalDateTime.of(2018, 10, 11, 12, 12)), None)(previousNow).get

    val toBeOnNextMonday = startOnFriday.next(startOnFriday.notificationDate)

    startOnFriday.notificationDate should be (LocalDateTime.of(2018, 10, 12, 12, 12))
    toBeOnNextMonday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 15, 12, 12)))
  }

  it should "respect given end date if periodic" in {
    val startAtWednesday = Periodic(12, 12, Set(0, 2, 4), None, Some(LocalDateTime.of(2018, 10, 11, 12, 12)))(previousNow).get

    val toBeOnFriday = startAtWednesday.next(startAtWednesday.notificationDate)

    startAtWednesday.notificationDate should be (LocalDateTime.of(2018, 10, 10, 12, 12))
    toBeOnFriday.map(_.notificationDate) should be (None)
  }

}
