package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.LegacyInternationalization


final class CombinatorMomentInFutureParser[T](ast: MomentInFutureAst[T], recAst: RecurrentAst[T]) extends MomentInFutureParser[T] {

  private val parser = new DurationParser(LegacyInternationalization, ast, recAst)

  override def parseMomentInFuture(string: String): Either[String, T] =
    parser.parse(string)
}
