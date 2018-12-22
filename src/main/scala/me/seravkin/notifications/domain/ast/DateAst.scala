package me.seravkin.notifications.domain.ast

trait DateAst[T] {
  def date(day: Int, month: Int, year: Int): T
  def date(day: Int, month: Int): T
}
