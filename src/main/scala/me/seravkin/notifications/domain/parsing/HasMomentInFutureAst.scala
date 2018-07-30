package me.seravkin.notifications.domain.parsing

trait HasMomentInFutureAst[T] {
  def momentInFutureAst: MomentInFutureAst[T]
}
