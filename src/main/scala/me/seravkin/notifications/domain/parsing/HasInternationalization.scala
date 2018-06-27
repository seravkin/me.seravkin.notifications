package me.seravkin.notifications.domain.parsing

import me.seravkin.notifications.domain.internationalization.Internationalization
import me.seravkin.notifications.domain.internationalization.Words.Word

import scala.util.parsing.combinator.RegexParsers

trait HasInternationalization { this: RegexParsers =>
  def internationalization: Internationalization

  def anyOf(word: Word): Parser[String] =
    internationalization.words(word).map(literal).reduce { _ | _ }
}
