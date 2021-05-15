package me.seravkin.notifications.test.interpreter

import java.time.{Duration, LocalDateTime}
import me.seravkin.notifications.domain.interpreter.Dates._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DatesSpec extends AnyFlatSpec with Matchers {

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
    Periodic(12, 12, Set(1), RecurrencyType.Week, None, None)(previousNow).get.notificationDate should be (LocalDateTime.of(2018, 10, 16, 12, 12))
  }

  it should "move correctly on next days of week if periodic" in {
    val startAtWednesday = Periodic(12, 12, Set(0, 2, 4), RecurrencyType.Week, None, None)(previousNow).get

    val toBeOnFriday = startAtWednesday.next(startAtWednesday.notificationDate)

    val toBeOnNextMonday = toBeOnFriday.flatMap(n => n.next(n.notificationDate))

    startAtWednesday.notificationDate should be (LocalDateTime.of(2018, 10, 10, 12, 12))
    toBeOnFriday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 12, 12, 12)))
    toBeOnNextMonday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 15, 12, 12)))
  }

  it should "move correctly on first day of month" in {
    val start = Periodic(12, 12, Set(0), RecurrencyType.Month, None, None)(previousNow).get

    start.notificationDate should be (LocalDateTime.of(2018, 11, 1, 12, 12))
  }

  it should "move correctly on next days of month if periodic" in {
    val startAtWedndesday = Periodic(12, 12, Set(14), RecurrencyType.Month, None, None)(previousNow).get

    val toBeInNovember = startAtWedndesday.next(startAtWedndesday.notificationDate)

    val toBeInDecember = toBeInNovember.flatMap(n => n.next(n.notificationDate))

    startAtWedndesday.notificationDate should be (LocalDateTime.of(2018, 10, 15, 12, 12))
    toBeInNovember.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 11, 15, 12, 12)))
    toBeInDecember.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 12, 15, 12, 12)))
  }

  it should "respect given start date if periodic" in {
    val startOnFriday = Periodic(12, 12, Set(0, 2, 4), RecurrencyType.Week, Some(LocalDateTime.of(2018, 10, 11, 12, 12)), None)(previousNow).get

    val toBeOnNextMonday = startOnFriday.next(startOnFriday.notificationDate)

    startOnFriday.notificationDate should be (LocalDateTime.of(2018, 10, 12, 12, 12))
    toBeOnNextMonday.map(_.notificationDate) should be (Some(LocalDateTime.of(2018, 10, 15, 12, 12)))
  }

  it should "respect given end date if periodic" in {
    val startAtWednesday = Periodic(12, 12, Set(0, 2, 4), RecurrencyType.Week, None, Some(LocalDateTime.of(2018, 10, 11, 12, 12)))(previousNow).get

    val toBeOnFriday = startAtWednesday.next(startAtWednesday.notificationDate)

    startAtWednesday.notificationDate should be (LocalDateTime.of(2018, 10, 10, 12, 12))
    toBeOnFriday.map(_.notificationDate) should be (None)
  }

}
