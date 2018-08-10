package me.seravkin.notifications.domain.internationalization

import me.seravkin.notifications.domain.internationalization.Words._

object LegacyInternationalization extends Internationalization {

  override def words(word: Words.Word): List[String] = word match {
    case Second => "секунда" :: "секунды" ::  "секунд" :: "сек" :: "c" :: "seconds" :: "second" :: "sec" :: "s" :: Nil
    case Minute => "минуты" :: "минута" :: "минут" :: "мин" :: "м" :: "minutes" :: "minute" :: "min" :: "m" :: Nil
    case Hour => "часов" :: "часа" :: "час" :: "ч" :: "hours" :: "hour" :: "h" :: Nil
    case Day => "дней" :: "день" :: "дня" :: "д":: "days" :: "sday" :: "d" :: Nil
    case Week => "неделю" :: "недели" :: "неделя" :: "недель" :: "нед" :: "week" :: "weeks" :: Nil
    case Monday => "понедельник" :: "пн" :: "monday" :: "mon" :: Nil
    case Tuesday => "вторник" :: "вт" :: "tuesday" :: "tue" :: Nil
    case Wednesday => "среда" :: "среду" :: "ср" :: "wednesday" :: "wed" :: Nil
    case Thursday => "четверг" :: "чт" :: "thursday" :: "thu" :: Nil
    case Friday => "пятница" :: "пятницу" :: "пт" :: "friday" :: "fri" :: Nil
    case Saturday => "суббота" :: "субботу" :: "сб" :: "saturday" :: "sat" :: Nil
    case Sunday => "воскресенье" :: "вс" :: "sunday" :: "sun" :: Nil
    case Every => "каждый" :: "каждые" :: "каждое" :: "каждую" :: Nil
    case In => "во" :: "в" :: Nil
    case CurrentTime => "это же время" :: "текущее время" :: "current time" :: Nil
    case EveryDayIn => "каждый день в" :: Nil
    case Today => "сегодня" :: "today" :: Nil
    case Tomorrow => "завтра" :: "tomorrow" :: Nil
    case DayAfterTomorrow => "послезавтра" :: "day after tomorrow" :: Nil
    case InAsInTime => "через" :: Nil
    case InWeek => "через неделю" :: Nil
    case WorkingDay => "будний день" :: "рабочий день" :: "будний" ::  "будни" :: Nil
    case Weekend => "выходные" :: "выходной день" :: "выходной" :: Nil
    case DateRegexString => """([0-9]{1,2})\.([0-9]{2})""" :: Nil
    case TimeRegexString => """([0-9]{1,2})\:([0-9]{2})""" :: Nil
    case FullDateRegexString => """([0-9]{1,2})\.([0-9]{2})\.([0-9]{4})""" :: Nil
    case ForUserString => "для" :: "for" :: Nil
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
    case AtNight => "ночью" :: Nil
    case AtMorning => "утром" :: Nil
    case AtEvening => "вечером" :: Nil
    case AtDay => "днем" :: Nil
    case Confirmation => "подтверждением" :: Nil
    case With => "с" :: Nil
  }

}
