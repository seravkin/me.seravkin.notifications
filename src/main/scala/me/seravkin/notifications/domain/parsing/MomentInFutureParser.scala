package me.seravkin.notifications.domain.parsing

trait MomentInFutureParser[T] {
  def parseMomentInFuture(string: String): Either[String, T]
}
