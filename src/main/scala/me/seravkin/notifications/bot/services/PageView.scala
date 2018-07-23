package me.seravkin.notifications.bot.services

import me.seravkin.notifications.domain.PersistedUser

trait PageView[F[_]] {
  def showPage(user: PersistedUser, chatId: Long, skip: Int, take: Int): F[Unit]
  def editPage(id: Int, user: PersistedUser, chatId: Long, skip: Int, take: Int): F[Unit]
}
