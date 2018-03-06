package me.seravkin.notifications.domain

final case class User(id: Long, chatId: Option[Long], username: String)
