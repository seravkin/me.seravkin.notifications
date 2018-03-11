package me.seravkin.notifications.domain.parsing



object CombinatorMomentInFutureParser extends MomentInFutureParser {

  private val parser = new DurationParser()

  override def parseMomentInFuture(string: String): Either[String, MomentInFuture] =
    parser.parse(string) match {
      case Right(x: MomentInFuture) => Right(x)
      case Right(other) => Left("Неподдерживаемый формат напоминания")
      case Left(error) => Left(error)
    }
}
