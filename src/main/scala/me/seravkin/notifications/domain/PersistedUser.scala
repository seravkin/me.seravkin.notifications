package me.seravkin.notifications.domain

final case class PersistedUser(id: Long, chatId: Option[Long], username: String)
