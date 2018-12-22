package me.seravkin.notifications.domain.ast

trait RelativeAst[T] {
  def dayOfWeek(weekOffset: Int, dayOfWeek: Int): T
  def inDays(days: Int): T
}
