package me.seravkin.notifications.domain.parsing

import java.time.Duration

trait MomentInFutureAst[T] {

  def dayOfWeek(weekOffset: Int, dayOfWeek: Int): T

  def inDays(days: Int): T

  def duration(duration: Duration): T

  def date(day: Int, month: Int, year: Int): T
  def date(day: Int, month: Int): T

  def time(hours: Int, minutes: Int): T
  def inCurrentTime: T

  def dateAndTime(date: T, time: T): T

  def fuzzyTime(period: Period): T

  def forUser(username: String, t: T): T
  def confirmation(duration: Option[Duration], t: T): T

}