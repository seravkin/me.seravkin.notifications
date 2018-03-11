package me.seravkin.notifications.test.parsing

import java.time.{Duration, LocalDateTime}

import me.seravkin.notifications.domain.parsing._
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class NotificationProgramSpec  extends FlatSpec with Matchers  {

  private[this] val now = LocalDateTime.of(2017,10,9,11,23)

  "Notification program" should "notify in 5 seconds" in {
    assertExecutionDateAndNotification(FromDuration(Duration.ofSeconds(5)), now.plusSeconds(5))
  }

  it should  "notify in 5 seconds and 10 hours" in {
    assertExecutionDateAndNotification(FromDuration(Duration.ofSeconds(5).plusHours(10)),
      now.plusSeconds(5).plusHours(10))
  }

  it should "notify in given date and time" in {
    assertExecutionDateAndNotification(FromFormattedDate(FormattedDate(22, 7), FormattedTime(17, 34)),
      now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))
  }

  it should "notify in days and given time" in {
    assertExecutionDateAndNotification(FromFormattedDate(InDays(0), FormattedTime(13, 45)),
      now.withHour(13).withMinute(45))
  }

  it should "notify in given date and current time" in {
    assertExecutionDateAndNotification(FromFormattedDate(FormattedDate(22, 7), InCurrentTime),
      now.withDayOfMonth(22).withMonth(7))
  }

  it should "notify in days and current time" in {
    assertExecutionDateAndNotification(FromFormattedDate(InDays(3), InCurrentTime),
      now.plusDays(3))
  }

  it should "notify in next day of week" in {
    assertExecutionDateAndNotification(FromFormattedDate(InNextDayOfWeek(0,1), InCurrentTime),
      now.plusDays(1))
  }

  it should "notify in that day of week on next week" in {
    assertExecutionDateAndNotification(FromFormattedDate(InNextDayOfWeek(0,0), InCurrentTime),
      now.plusWeeks(1))
  }

  it should "notify in next day of week and time" in {
    assertExecutionDateAndNotification(FromFormattedDate(InNextDayOfWeek(0,1), FormattedTime(12, 0)),
      now.plusDays(1).withHour(12).withMinute(0))
  }

  it should "notify in next week's wednesday and current time" in {
    assertExecutionDateAndNotification(FromFormattedDate(InNextDayOfWeek(1,2), InCurrentTime),
      now.plusWeeks(1).plusDays(2))
  }

  it should "notify in 2 weeks on tuesday" in {
    assertExecutionDateAndNotification(FromFormattedDate(InNextDayOfWeek(2,3), FormattedTime(12, 0)),
      now.plusWeeks(2).plusDays(3).withHour(12).withMinute(0))
  }

  it should "notify in given date and time for specific user" in {
    assertNotification(ForUser("DogeShibu", FromFormattedDate(FormattedDate(22, 7), FormattedTime(17, 34))),
      now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))
  }


  private[this] def assertExecutionDateAndNotification(momentInFuture: MomentInFuture,
                                                       expectedExecutionTime: LocalDateTime) = {

    val time = momentInFuture.toExecutionTime(now)

    time should be (expectedExecutionTime)

    momentInFuture.shouldNotify(now, expectedExecutionTime.plusSeconds(10)) should be (NotifyAndStop)
  }

  private[this] def assertNotification(momentInFuture: NotificationProgram,
                                       expectedExecutionTime: LocalDateTime) = {

    momentInFuture.shouldNotify(now, expectedExecutionTime.plusSeconds(10)).shouldExecute should be (true)
  }
}
