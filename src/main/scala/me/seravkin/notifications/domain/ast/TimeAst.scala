package me.seravkin.notifications.domain.ast

import me.seravkin.notifications.domain.parsing.Period

trait TimeAst[T] {
  def fuzzyTime(period: Period): T
  def time(hours: Int, minutes: Int): T
  def inCurrentTime: T
}
