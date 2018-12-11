package me.seravkin.notifications.domain.parsing

import atto._
import atto.syntax.all._
import me.seravkin.notifications.domain.internationalization.Internationalization
import me.seravkin.notifications.domain.internationalization.Words.Word

trait InternalizationParsers {
  def anyOf(word: Word): Parser[String]

  def fullDateRegex: Parser[(Int, Int, Int)]
  def timeRegex: Parser[(Int, Int)]
  def dateRegex: Parser[(Int, Int)]
}

object InternalizationParsers {

  def apply(internationalization: Internationalization): InternalizationParsers = new InternalizationParsers {
    override def anyOf(word: Word): Parser[String] =
      internationalization.words(word).map(Atto.string).reduce { _ | _ }

    override def fullDateRegex: Parser[(Int, Int, Int)] =
      Atto.int ~ Atto.char('.') ~ Atto.int ~ Atto.char('.') ~ Atto.int -| { case ((((d, _), m), _),y) =>
        (d, m, y)
      }
    override def timeRegex: Parser[(Int, Int)] = Atto.int ~ Atto.char(':') ~ Atto.int -| { case ((h, _), m) =>
      (h, m)
    }
    override def dateRegex: Parser[(Int, Int)] = Atto.int ~ Atto.char('.') ~ Atto.int -| { case ((d, _), m) =>
      (d, m)
    }
  }
}
