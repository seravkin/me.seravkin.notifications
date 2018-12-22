package me.seravkin.notifications.domain.ast

trait DateAndTimeAst[T, D, R] {
  def dateAndTime(date: D, time: R): T
}
