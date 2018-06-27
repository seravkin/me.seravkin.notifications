package me.seravkin.notifications.domain.internationalization

import me.seravkin.notifications.domain.internationalization.Words.Word

trait Internationalization {
  def words(word: Word): List[String]
}
