package me.seravkin.notifications.domain.ast

trait UserAst[T] {
  def forUser(username: String, t: T): T
}
