package me.seravkin.notifications.persistance

case class Page[+T](contents: List[T], hasPrevious: Boolean, hasNext: Boolean)
