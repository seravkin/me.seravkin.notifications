package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.LegacyInternationalization
import me.seravkin.notifications.domain.ast._

final class CombinatorMomentInFutureParser[T](durationAst: DurationAst[T],
                                              timeAst: TimeAst[T],
                                              dateAst: DateAst[T],
                                              dateAndTimeAst: DateAndTimeAst[T, T, T],
                                              relativeAst: RelativeAst[T],
                                              userAst: UserAst[T],
                                              confirmationAst: ConfirmationAst[T],
                                              recAst: RecurrentAst[T]) extends MomentInFutureParser[T] {

  private[this] val intl = InternalizationParsers(LegacyInternationalization)
  private[this] val timeConstants = new TimeConstants(intl)
  private[this] val commonParsers = new CommonParsers[T](intl, timeConstants, durationAst, dateAst, timeAst)
  private[this] val dateParsers = new RelativeParsers[T](intl, commonParsers, timeConstants, relativeAst)
  private[this] val recurrentParsers = new RecurrentParsers[T, T](intl, commonParsers, recAst)
  private[this] val timeParsers = new TimeParsers[T](intl, timeConstants, dateParsers, commonParsers,
    timeAst,
    dateAndTimeAst,
    relativeAst)

  private[this] val parser = new DurationParser(
    intl,
    commonParsers,
    timeParsers,
    recurrentParsers,
    confirmationAst,
    userAst,
    recAst
  )

  override def parseMomentInFuture(string: String): Either[String, T] =
    parser.parse(string)
}
