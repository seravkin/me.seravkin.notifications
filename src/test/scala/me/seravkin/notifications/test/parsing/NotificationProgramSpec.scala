package me.seravkin.notifications.test.parsing

import java.time.{Duration, LocalDateTime}

import cats.Id
import me.seravkin.notifications.domain.interpreter.Dates.OneDate
import me.seravkin.notifications.domain.interpreter.DatesAst
import me.seravkin.notifications.domain.parsing.Period.Morning
import org.scalatest.{FlatSpec, Matchers}

class NotificationProgramSpec  extends FlatSpec with Matchers  {

  private[this] val now = LocalDateTime.of(2017,10,9,11,23)

  private[this] val ast = new DatesAst[Id]((int: Int) => new util.Random(1).nextInt(int))

  "Notification program" should "notify in 5 seconds" in {
    ast.duration(Duration.ofSeconds(5))(now) should equal (Right(OneDate(now.plusSeconds(5))))
  }

  it should  "notify in 5 seconds and 10 hours" in {
    ast.duration(Duration.ofSeconds(5).plusHours(10))(now) should equal (Right(OneDate(now.plusSeconds(5).plusHours(10))))
  }

  it should "notify in given date and time" in {
    ast.dateAndTime(ast.date(22, 7), ast.time(17,34))(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

  it should "notify in given date with year and time" in {
    ast.dateAndTime(ast.date(22, 7, 2017), ast.time(17,34))(now) should
      equal (Right(OneDate(now.withYear(2017).withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

  it should "notify in days and given time" in {
    ast.dateAndTime(ast.inDays(0), ast.time(13, 45))(now) should
      equal (Right(OneDate(now.withHour(13).withMinute(45))))
  }

  it should "notify in given date and current time" in {
    ast.dateAndTime(ast.date(22, 7), ast.inCurrentTime)(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7))))
  }

  it should "notify in days and current time" in {
    ast.dateAndTime(ast.inDays(3), ast.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusDays(3))))
  }

  it should "notify in days and fuzzy time" in {
    ast.dateAndTime(ast.inDays(3), ast.fuzzyTime(Morning))(now) should
      equal (Right(OneDate(now.plusDays(3).withHour(8).withMinute(25))))
  }

  it should "notify in next day of week" in {
    ast.dateAndTime(ast.dayOfWeek(0, 1), ast.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusDays(1))))
  }

  it should "notify in that day of week on next week" in {
    ast.dateAndTime(ast.dayOfWeek(0, 0), ast.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusWeeks(1))))
  }

  it should "notify in next day of week and time" in {
    ast.dateAndTime(ast.dayOfWeek(0, 1), ast.time(12, 0))(now) should
      equal (Right(OneDate(now.plusDays(1).withHour(12).withMinute(0))))
  }

  it should "notify in next week's wednesday and current time" in {
    ast.dateAndTime(ast.dayOfWeek(1, 2), ast.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusWeeks(1).plusDays(2))))
  }

  it should "notify in 2 weeks on tuesday" in {
    ast.dateAndTime(ast.dayOfWeek(2, 3), ast.time(12, 0))(now) should
      equal (Right(OneDate(now.plusWeeks(2).plusDays(3).withHour(12).withMinute(0))))
  }

  it should "notify in given date and time for specific user" in {
    ast.forUser("DogeShibu", ast.dateAndTime(ast.date(22, 7), ast.time(17, 34)))(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

}
