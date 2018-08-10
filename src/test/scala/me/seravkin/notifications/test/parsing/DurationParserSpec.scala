package me.seravkin.notifications.test.parsing

import java.time.Duration

import me.seravkin.notifications.domain.internationalization.LegacyInternationalization
import me.seravkin.notifications.domain.interpreter.NotificationProgramAst
import me.seravkin.notifications.domain.parsing._
import org.scalatest.{FlatSpec, FunSuite, Matchers}

class DurationParserSpec extends FlatSpec with Matchers {

  "DurationParser" should "parse simple message" in {
    assertParsedTime(FromDuration(Duration.ofSeconds(5)))("5 sec")
  }

  it should "parse simple message with multiple durations" in {
    assertParsedTime(FromDuration(Duration.ofSeconds(5).plusHours(10)))("5 seconds 10 hours")
  }

  it should "parse simple message with multiple durations in russian" in {
    assertParsedTime(FromDuration(Duration.ofSeconds(5).plusHours(10)))("5 секунд 10 часов")
  }

  it should "parse simple date" in {
    assertParsedTime(FromFormattedDate(FormattedDate(22, 7), FormattedTime(12, 0)))("22.07 в 12:00")
  }

  it should "parse simple date with fuzzy time" in {
    assertParsedTime(FromFormattedDate(FormattedDate(22, 7), AtFuzzyTime(Evening)))("22.07 вечером")
  }

  it should "parse date with full" in {
    assertParsedTime(FromFormattedDate(FormattedDateWithYear(22, 7, 2017), FormattedTime(12, 0)))("22.07.2017 в 12:00")
  }

  it should "parse today date and time" in {
    assertParsedTime(FromFormattedDate(InDays(0), FormattedTime(12, 45)))("сегодня в 12:45")
  }

  it should "parse today date and time with confirmation" in {
    assertParsedTime(WithConfirmation(None, FromFormattedDate(InDays(0), FormattedTime(12, 45))))("сегодня в 12:45 с подтверждением")
  }

  it should "parse today date and time with given duration" in {
    assertParsedTime(WithConfirmation(Some(Duration.ofMinutes(30)), FromFormattedDate(InDays(0), FormattedTime(12, 45))))("сегодня в 12:45 с подтверждением каждые 30 м")
  }

  it should "parse today date and time in short format" in {
    assertParsedTime(FromFormattedDate(InDays(0), FormattedTime(12, 45)))("12:45")
  }

  it should "parse today date and time ignoring case" in {
    assertParsedTime(FromFormattedDate(InDays(0), FormattedTime(12, 45)))("СеГоднЯ В 12:45")
  }

  it should "parse date and current time" in {
    assertParsedTime(FromFormattedDate(FormattedDate(22, 7), InCurrentTime))("22.07 в это же время")
  }

  it should "parse in days and current time" in {
    assertParsedTime(FromFormattedDate(InDays(3), InCurrentTime))("через 3 дня в это же время")
  }

  it should "parse in days and current time with numerals" in {
    assertParsedTime(FromFormattedDate(InDays(3), InCurrentTime))("через три дня в это же время")
  }

  it should "parse in day of week" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(0,1), InCurrentTime))("во вторник в это же время")
  }

  it should "parse in day of week at fuzzy time" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(0,1), AtFuzzyTime(Morning)))("во вторник утром")
  }

  it should "parse in day of week with time" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(0,1), FormattedTime(12, 0)))("во вторник в 12:00")
  }

  it should "parse in day of week on next week" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(1,2), InCurrentTime))("в среду через неделю в это же время")
  }

  it should "parse in day of week with time on next week" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(2,3), FormattedTime(12, 0)))("в чт через 2 нед в 12:00")
  }

  it should "parse in day of week with time on next week with numerals" in {
    assertParsedTime(FromFormattedDate(InNextDayOfWeek(2,3), FormattedTime(12, 0)))("в чт через две нед в 12:00")
  }

  it should "parse notification for specific user" in {
    assertParsedTime(ForUser("DogeShibu", FromFormattedDate(InDays(0), FormattedTime(12, 45))))("для @DogeShibu сегодня в 12:45")
  }

  it should "parse on every day in given time" in {
    assertParsedTime(InTime(12, 0))("каждый день в 12:00")
  }

  it should "parse on wednesday in given time" in {
    assertParsedTime(EveryDaysOfWeek(Set(2), InTime(13, 45)))("каждую среду в 13:45")
  }

  it should "parse on multiple days of week in given time" in {
    assertParsedTime(EveryDaysOfWeek(Set(2, 3), InTime(13, 45)))("каждую среду, четверг в 13:45")
  }

  it should "parse on working days of week in given time" in {
    assertParsedTime(EveryDaysOfWeek(Set(0, 1, 2, 3, 4), InTime(13, 45)))("каждый будний в 13:45")
  }

  it should "parse on weekends of week in given time" in {
    assertParsedTime(EveryDaysOfWeek(Set(5, 6), InTime(13, 45)))("каждый выходной в 13:45")
  }

  private def assertParsedTime(momentInFuture: NotificationProgram)(text: String) = {
    val parser = new CombinatorMomentInFutureParser[NotificationProgram](NotificationProgramAst, NotificationProgramAst)
    val result = parser.parseMomentInFuture(text)

    result should be (Right(momentInFuture))
  }
}

