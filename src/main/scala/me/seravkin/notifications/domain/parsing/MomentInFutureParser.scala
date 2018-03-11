package me.seravkin.notifications.domain.parsing

trait MomentInFutureParser {
  def parseMomentInFuture(string: String): Either[String, MomentInFuture]
}
