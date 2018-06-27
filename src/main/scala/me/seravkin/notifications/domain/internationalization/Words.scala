package me.seravkin.notifications.domain.internationalization

object Words {

  sealed trait Word

  object Second extends Word
  object Minute extends Word
  object Hour extends Word
  object Day extends Word
  object Week extends Word

  sealed trait DayOfWeek {
    def toInt: Int
  }

  object Monday extends Word with DayOfWeek { override def toInt: Int = 0 }
  object Tuesday extends Word with DayOfWeek { override def toInt: Int = 1 }
  object Wednesday extends Word with DayOfWeek { override def toInt: Int = 2 }
  object Thursday extends Word with DayOfWeek { override def toInt: Int = 3 }
  object Friday extends Word with DayOfWeek { override def toInt: Int = 4 }
  object Saturday extends Word with DayOfWeek { override def toInt: Int = 5 }
  object Sunday extends Word with DayOfWeek { override def toInt: Int = 6 }

  object Every extends Word
  object In extends Word

  object CurrentTime extends Word
  object AtNight extends Word
  object AtMorning extends Word
  object AtDay extends Word
  object AtEvening extends Word

  object EveryDayIn extends Word
  object Today extends Word
  object Tomorrow extends Word
  object DayAfterTomorrow extends Word
  object InAsInTime extends Word
  object InWeek extends Word

  object WorkingDay extends Word
  object Weekend extends Word

  object DateRegexString extends Word
  object TimeRegexString extends Word
  object FullDateRegexString extends Word

  object ForUserString extends Word

  final case class Numeral(value: Int) extends Word

}
