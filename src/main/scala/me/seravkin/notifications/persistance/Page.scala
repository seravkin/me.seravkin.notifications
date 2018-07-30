package me.seravkin.notifications.persistance

final case class Page[+T](contents: List[T], hasPrevious: Boolean, hasNext: Boolean)
