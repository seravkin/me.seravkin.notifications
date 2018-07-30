package me.seravkin.notifications.domain.interpreter

import cats._
import cats.implicits._
import cats.data._
import java.time.{Duration, LocalDateTime}

import me.seravkin.notifications.domain.parsing
import me.seravkin.notifications.domain.parsing.{HasMomentInFutureAst, MomentInFutureAst}
import me.seravkin.notifications.infrastructure.random.Random


final class MomentInFutureAstToF[F[_]: Applicative](random: Random[F]) extends MomentInFutureAst[NotificationPrototype[F]] {

  override def dayOfWeek(weekOffset: Int, dayOfWeek: Int): NotificationPrototype[F] = readerOfF() { now =>
    val isAfterGivenDayOfWeek = now.getDayOfWeek.ordinal() >= dayOfWeek
    val trueWeekOffset  = if(isAfterGivenDayOfWeek) weekOffset + 1 else weekOffset

    now.minusDays(now.getDayOfWeek.ordinal()).plusDays(7 * trueWeekOffset).plusDays(dayOfWeek)
  }

  override def inDays(days: Int): NotificationPrototype[F] = readerOfF(isRelative = true) { now =>
    now.withHour(0).withMinute(0).plusDays(days)
  }

  override def duration(duration: Duration): NotificationPrototype[F] = readerOfF() { now =>
    now.plus(duration)
  }

  override def date(day: Int, month: Int, year: Int): NotificationPrototype[F] = readerOfF() { now =>
    LocalDateTime.of(year, month, day, 0, 0)
  }

  override def date(day: Int, month: Int): NotificationPrototype[F] = readerOfF() { now =>
    LocalDateTime.of(now.getYear, month, day, 0, 0)
  }

  override def time(hours: Int, minutes: Int): NotificationPrototype[F] = readerOfF() { now =>
    now.withHour(hours).withMinute(minutes)
  }

  override def inCurrentTime: NotificationPrototype[F] = readerOfF() { now =>
    now
  }

  override def dateAndTime(date: NotificationPrototype[F], time: NotificationPrototype[F]): NotificationPrototype[F] =
    readerOf(date.userName.orElse(time.userName), date.isRelative || time.isRelative) { now =>
      date(now).map2(time(now)) { case (dt, tm) =>
        dt.withHour(tm.getHour).withMinute(tm.getMinute)
      }
    }

  override def fuzzyTime(period: parsing.Period): NotificationPrototype[F] = readerOf() { now =>
    val (start, end) = period.period

    random.nextInt(end - start + 1).map2(random.nextInt(8)) { case (deltaHour, fiveMin) =>
      now.withHour(start + deltaHour).withMinute(fiveMin * 5)
    }
  }

  override def forUser(username: String, t: NotificationPrototype[F]): NotificationPrototype[F] =
    t.copy(userName = Some(username))

  private[this] def readerOfF(username: Option[String] = None, isRelative: Boolean = false)(f: LocalDateTime => LocalDateTime) =
    NotificationPrototype(None, isRelative, ReaderT(d => Applicative[F].pure(f(d))))

  private[this] def readerOf(username: Option[String] = None, isRelative: Boolean = false)(f: LocalDateTime => F[LocalDateTime]) =
    NotificationPrototype(None, isRelative, ReaderT(f))
}
