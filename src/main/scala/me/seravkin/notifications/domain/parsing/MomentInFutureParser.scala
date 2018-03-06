package me.seravkin.notifications.domain.parsing

trait MomentInFutureParser {
  def parse(string: String): Either[String, MomentInFuture]
}
