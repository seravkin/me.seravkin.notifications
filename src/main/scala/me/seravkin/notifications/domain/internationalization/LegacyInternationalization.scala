package me.seravkin.notifications.domain.internationalization

import me.seravkin.notifications.domain.internationalization.Words._

object LegacyInternationalization extends Internationalization {

  private[this] type Translator = PartialFunction[Words.Word, List[String]]

  private[this] val months: Translator = {
    case Month.January => "января" :: "янв" :: "january" :: "jan" :: Nil
    case Month.February => "февраля" :: "фев" :: "february" :: "feb" :: Nil
    case Month.March => "марта" :: "мар" :: "march" :: "mar" :: Nil
    case Month.April => "апреля" :: "апр" :: "april" :: "apr" :: Nil
    case Month.May => "мая" :: "май" :: "may" :: "may" :: Nil
    case Month.June => "июня" :: "июн" :: "june" :: "jun" :: Nil
    case Month.July => "июля" :: "июл" :: "july" :: "jul" :: Nil
    case Month.August => "август" :: "авг" :: "august" :: "aug" :: Nil
    case Month.September => "сентября" :: "сен" :: "september" :: "sept" :: Nil
    case Month.October => "октября" :: "окт" :: "october" :: "oct" :: Nil
    case Month.November => "ноября" :: "ноя" :: "november" :: "nov" :: Nil
    case Month.December => "декабря" :: "дек" :: "december" :: "dec" :: Nil
  }

  private[this] val days: Translator = {
    case DayOfWeek.Monday => "понедельник" :: "пн" :: "monday" :: "mon" :: Nil
    case DayOfWeek.Tuesday => "вторник" :: "вт" :: "tuesday" :: "tue" :: Nil
    case DayOfWeek.Wednesday => "среда" :: "среду" :: "ср" :: "wednesday" :: "wed" :: Nil
    case DayOfWeek.Thursday => "четверг" :: "чт" :: "thursday" :: "thu" :: Nil
    case DayOfWeek.Friday => "пятница" :: "пятницу" :: "пт" :: "friday" :: "fri" :: Nil
    case DayOfWeek.Saturday => "суббота" :: "субботу" :: "сб" :: "saturday" :: "sat" :: Nil
    case DayOfWeek.Sunday => "воскресенье" :: "вс" :: "sunday" :: "sun" :: Nil
  }

  private[this] val durations: Translator = {
    case Second => "секунда" :: "секунды" ::  "секунд" :: "сек" :: "c" :: "seconds" :: "second" :: "sec" :: "s" :: Nil
    case Minute => "минуты" :: "минута" :: "минут" :: "мин" :: "м" :: "minutes" :: "minute" :: "min" :: "m" :: Nil
    case Hour => "часов" :: "часа" :: "час" :: "ч" :: "hours" :: "hour" :: "h" :: Nil
    case Day => "число" :: "числа" :: "дней" :: "день" :: "дня" :: "д":: "days" :: "sday" :: "d" :: Nil
    case Week => "неделю" :: "недели" :: "неделя" :: "недель" :: "нед" :: "week" :: "weeks" :: Nil
    case MonthPeriod => "месяц" :: "месяца" :: "month" :: Nil
  }

  private[this] val numerals: Translator = {
    case Numeral(0) => "ноль" :: Nil
    case Numeral(1) => "один" :: "одну" :: Nil
    case Numeral(2) => "два" :: "две" :: Nil
    case Numeral(3) => "три" :: Nil
    case Numeral(4) => "четыре" :: Nil
    case Numeral(5) => "пять" :: Nil
    case Numeral(6) => "шесть" :: Nil
    case Numeral(7) => "семь" :: Nil
    case Numeral(8) => "восемь" :: Nil
    case Numeral(9) => "девять" :: Nil
    case Numeral(i) => i.toString :: Nil
  }

  private[this] val prepositions: Translator = {
    case Every => "каждый" :: "каждые" :: "каждое" :: "каждую" :: Nil
    case In => "во" :: "в" :: Nil
    case CurrentTime => "это же время" :: "текущее время" :: "current time" :: Nil
    case EveryDayIn => "каждый день в" :: Nil
    case EveryMonthIn => "каждый месяц" :: Nil
    case InAsInTime => "через" :: Nil
    case ForUserString => "для" :: "for" :: Nil
    case With => "с" :: Nil
  }

  private[this] val times: Translator = {
    case AtNight => "ночью" :: Nil
    case AtMorning => "утром" :: Nil
    case AtEvening => "вечером" :: Nil
    case AtDay => "днем" :: Nil
  }

  private[this] val phrases: Translator = {
    case Today => "сегодня" :: "today" :: Nil
    case Tomorrow => "завтра" :: "tomorrow" :: Nil
    case DayAfterTomorrow => "послезавтра" :: "day after tomorrow" :: Nil
    case Confirmation => "подтверждением" :: Nil
    case InWeek => "через неделю" :: Nil
    case WorkingDay => "будний день" :: "рабочий день" :: "будний" ::  "будни" :: Nil
    case Weekend => "выходные" :: "выходной день" :: "выходной" :: Nil
  }

  private[this] val regexes: Translator = {
    case DateRegexString => """([0-9]{1,2})\.([0-9]{2})""" :: Nil
    case TimeRegexString => """([0-9]{1,2})\:([0-9]{2})""" :: Nil
    case FullDateRegexString => """([0-9]{1,2})\.([0-9]{2})\.([0-9]{4})""" :: Nil
  }

  override def words(word: Words.Word): List[String] =
    months
      .orElse(days)
      .orElse(durations)
      .orElse(numerals)
      .orElse(times)
      .orElse(phrases)
      .orElse(regexes)
      .orElse(prepositions)
      .apply(word)

}
