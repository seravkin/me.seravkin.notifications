package me.seravkin.notifications.domain.ast

trait RecurrentAst[T] {
  def everyDayOfMonth(days: Set[Int], t: T): T
  def everyDayOfWeek(days: Set[Int], t: T): T
  def before(day: Int, month: Int, year: Option[Int], recurrent: T): T
  def after(day: Int, month: Int, year: Option[Int], recurrent: T): T
  def inTime(hours: Int, minutes: Int): T
}
