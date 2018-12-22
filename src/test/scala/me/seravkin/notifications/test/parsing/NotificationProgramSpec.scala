package me.seravkin.notifications.test.parsing

import java.time.{Duration, LocalDateTime}

import cats.data.ReaderT
import cats.mtl.implicits._
import cats.instances.either._
import me.seravkin.notifications.domain.interpreter.Dates.OneDate
import me.seravkin.notifications.domain.interpreter._
import me.seravkin.notifications.domain.parsing.Period.Morning
import org.scalatest.{FlatSpec, Matchers}

class NotificationProgramSpec  extends FlatSpec with Matchers  {

  private[this] type AstF[A] = ReaderT[Either[String, ?], LocalDateTime, A]
  private[this] val now = LocalDateTime.of(2017,10,9,11,23)

  private[this] val durationAst = new DurationApplicativeAst[AstF]
  private[this] val dateAst = new DateApplicativeAst[AstF]
  private[this] val timeAst = new TimeApplicativeAst[AstF](
    (i: Int) => ReaderT[Either[String, ?], LocalDateTime, Int]
      (_ => Right(new util.Random(1).nextInt(i)))
  )
  private[this] val relativeAst = new RelativeApplicativeAst[AstF]
  private[this] val dateAndTimeAst = new DateAndTimeApplicativeAst[AstF]
  private[this] val userAst = new UserApplicativeAst[AstF]

  "Notification program" should "notify in 5 seconds" in {
    durationAst.duration(Duration.ofSeconds(5))(now) should equal (Right(OneDate(now.plusSeconds(5))))
  }

  it should  "notify in 5 seconds and 10 hours" in {
    durationAst.duration(Duration.ofSeconds(5).plusHours(10))(now) should equal (Right(OneDate(now.plusSeconds(5).plusHours(10))))
  }

  it should "notify in given date and time" in {
    dateAndTimeAst.dateAndTime(dateAst.date(22, 7), timeAst.time(17,34))(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

  it should "notify in given date with year and time" in {
    dateAndTimeAst.dateAndTime(dateAst.date(22, 7, 2017), timeAst.time(17,34))(now) should
      equal (Right(OneDate(now.withYear(2017).withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

  it should "notify in days and given time" in {
    dateAndTimeAst.dateAndTime(relativeAst.inDays(0), timeAst.time(13, 45))(now) should
      equal (Right(OneDate(now.withHour(13).withMinute(45))))
  }

  it should "notify in given date and current time" in {
    dateAndTimeAst.dateAndTime(dateAst.date(22, 7), timeAst.inCurrentTime)(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7))))
  }

  it should "notify in days and current time" in {
    dateAndTimeAst.dateAndTime(relativeAst.inDays(3), timeAst.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusDays(3))))
  }

  it should "notify in days and fuzzy time" in {
    dateAndTimeAst.dateAndTime(relativeAst.inDays(3), timeAst.fuzzyTime(Morning))(now) should
      equal (Right(OneDate(now.plusDays(3).withHour(8).withMinute(25))))
  }

  it should "notify in next day of week" in {
    dateAndTimeAst.dateAndTime(relativeAst.dayOfWeek(0, 1), timeAst.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusDays(1))))
  }

  it should "notify in that day of week on next week" in {
    dateAndTimeAst.dateAndTime(relativeAst.dayOfWeek(0, 0), timeAst.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusWeeks(1))))
  }

  it should "notify in next day of week and time" in {
    dateAndTimeAst.dateAndTime(relativeAst.dayOfWeek(0, 1), timeAst.time(12, 0))(now) should
      equal (Right(OneDate(now.plusDays(1).withHour(12).withMinute(0))))
  }

  it should "notify in next week's wednesday and current time" in {
    dateAndTimeAst.dateAndTime(relativeAst.dayOfWeek(1, 2), timeAst.inCurrentTime)(now) should
      equal (Right(OneDate(now.plusWeeks(1).plusDays(2))))
  }

  it should "notify in 2 weeks on tuesday" in {
    dateAndTimeAst.dateAndTime(relativeAst.dayOfWeek(2, 3), timeAst.time(12, 0))(now) should
      equal (Right(OneDate(now.plusWeeks(2).plusDays(3).withHour(12).withMinute(0))))
  }

  it should "notify in given date and time for specific user" in {
    userAst.forUser("DogeShibu",
      dateAndTimeAst.dateAndTime(dateAst.date(22, 7), timeAst.time(17, 34)))(now) should
      equal (Right(OneDate(now.withDayOfMonth(22).withMonth(7).withHour(17).withMinute(34))))
  }

}
