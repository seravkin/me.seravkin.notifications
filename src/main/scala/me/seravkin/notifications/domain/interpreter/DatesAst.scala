package me.seravkin.notifications.domain.interpreter

import java.time.{Duration, LocalDateTime}

import cats._
import cats.implicits._
import cats.data._
import me.seravkin.notifications.domain.parsing
import me.seravkin.notifications.domain.parsing.{MomentInFutureAst, RecurrentAst}
import me.seravkin.notifications.infrastructure.random.Random

final class DatesAst[F[_]: Applicative](random: Random[F]) extends
  RecurrentAst[DatesFactory[F]] with MomentInFutureAst[DatesFactory[F]] {

  override def everyDayOfWeek(days: Set[Int], recurrent: DatesFactory[F]): DatesFactory[F] =
    recurrent.map(_.map {
      case p: Periodic => p.copy(days = days)
      case other => other
    })

  override def before(day: Int, month: Int, year: Option[Int], recurrent: DatesFactory[F]): DatesFactory[F] = ReaderT(now =>
    recurrent.map(_.map {
      case p: Periodic => p.copy(end = Some(LocalDateTime.of(year.getOrElse(now.getYear), month, day, 0, 0)))
      case other => other
    }).apply(now)
  )

  override def after(day: Int, month: Int, year: Option[Int], recurrent: DatesFactory[F]): DatesFactory[F] = ReaderT(now =>
    recurrent.map(_.map {
      case p: Periodic => p.copy(start = Some(LocalDateTime.of(year.getOrElse(now.getYear), month, day, 0, 0)))
      case other => other
    }).apply(now))

  override def inTime(hours: Int, minutes: Int): DatesFactory[F] = ReaderT(now => Applicative[F].pure {
      Periodic(hours, minutes, (0 until 7).toSet, None, None)(now) match {
        case Some(periodic) => Right(periodic)
        case None => Left(s"Напоминание не будет запущено т.к. $now позже даты конца напоминаний")
      }
    }
  )

  override def dayOfWeek(weekOffset: Int, dayOfWeek: Int): DatesFactory[F] = factoryFor { now =>
    val isAfterGivenDayOfWeek = now.getDayOfWeek.ordinal() >= dayOfWeek
    val trueWeekOffset  = if(isAfterGivenDayOfWeek) weekOffset + 1 else weekOffset

    now.minusDays(now.getDayOfWeek.ordinal()).plusDays(7 * trueWeekOffset).plusDays(dayOfWeek)
  }

  override def inDays(days: Int): DatesFactory[F] = factoryFor { now =>
    now.withHour(0).withMinute(0).plusDays(days)
  }

  override def duration(duration: Duration): DatesFactory[F] = factoryFor { now =>
    now.plus(duration)
  }

  override def date(day: Int, month: Int, year: Int): DatesFactory[F] = factoryFor { now =>
    LocalDateTime.of(year, month, day, 0, 0)
  }

  override def date(day: Int, month: Int): DatesFactory[F] = factoryFor { now =>
    LocalDateTime.of(now.getYear, month, day, 0, 0)
  }

  override def time(hours: Int, minutes: Int): DatesFactory[F] = factoryFor { now =>
    now.withHour(hours).withMinute(minutes)
  }

  override def inCurrentTime: DatesFactory[F] = factoryFor(identity)

  override def dateAndTime(date: DatesFactory[F], time: DatesFactory[F]): DatesFactory[F] =
    (date, time).mapN {
      case (Right(OneDate(dt)), Right(OneDate(tm))) =>
        Right(OneDate(dt.withHour(tm.getHour).withMinute(tm.getMinute)))
      case _ =>
        Left(s"Ошибка типов для $date и $time")
    }

  override def fuzzyTime(period: parsing.Period): DatesFactory[F] = ReaderT { now =>
    val (start, end) = period.period

    random.nextInt(end - start + 1).map2(random.nextInt(8)) { case (deltaHour, fiveMin) =>
      Right(OneDate(now.withHour(start + deltaHour).withMinute(fiveMin * 5)))
    }
  }

  override def forUser(username: String, t: DatesFactory[F]): DatesFactory[F] = t

  override def confirmation(duration: Option[Duration], t: DatesFactory[F]): DatesFactory[F] =
    t.map {
      case Right(OneDate(dt)) =>
        Right(Confirmation(dt, duration.getOrElse(Duration.ofMinutes(5))))
      case Right(value) =>
        Left(s"Ошибка типа для $value")
      case other => other
    }

  private[this] def factoryFor(f: LocalDateTime => LocalDateTime): DatesFactory[F] = ReaderT(now =>
    Applicative[F].pure(Right(OneDate(f(now))))
  )

}
