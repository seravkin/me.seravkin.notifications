package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.LegacyInternationalization

final class CombinatorMomentInFutureParser[T](ast: MomentInFutureAst[T], recAst: RecurrentAst[T]) extends MomentInFutureParser[T] {

  private[this] val intl = InternalizationParsers(LegacyInternationalization)
  private[this] val timeConstants = new TimeConstants(intl)
  private[this] val commonParsers = new CommonParsers[T](intl, timeConstants, ast)
  private[this] val dateParsers = new DateParsers[T](intl, commonParsers, timeConstants, ast)
  private[this] val recurrentParsers = new RecurrentParsers[T, T](intl, commonParsers, recAst)
  private[this] val timeParsers = new TimeParsers[T](intl, timeConstants, dateParsers, commonParsers, ast)

  private[this] val parser = new DurationParser(
    intl,
    commonParsers,
    timeParsers,
    recurrentParsers,
    ast,
    recAst
  )

  override def parseMomentInFuture(string: String): Either[String, T] =
    parser.parse(string)
}
