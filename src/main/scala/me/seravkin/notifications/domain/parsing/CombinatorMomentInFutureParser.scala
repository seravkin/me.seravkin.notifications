package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.LegacyInternationalization


object CombinatorMomentInFutureParser extends MomentInFutureParser {

  private val parser = new DurationParser(LegacyInternationalization)

  override def parseMomentInFuture(string: String): Either[String, MomentInFuture] =
    parser.parse(string) match {
      case Right(x: MomentInFuture) => Right(x)
      case Right(other) => Left("Неподдерживаемый формат напоминания")
      case Left(error) => Left(error)
    }
}
