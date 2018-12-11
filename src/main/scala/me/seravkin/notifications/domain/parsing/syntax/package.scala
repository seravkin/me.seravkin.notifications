package me.seravkin.notifications.domain.parsing

import atto._
import atto.parser.Combinator
import atto.syntax.all._
import cats.data.NonEmptyList

package object syntax {
  object all {

    private def ws = Atto.many(Atto.whitespace)
    private def ws1 = Atto.many1(Atto.whitespace)

    implicit class AdditionalAttoOps(comb: Combinator) {
      def optWs[A](m: Parser[A]): Parser[Option[A]] =
        Atto.opt(m)

      def ws1: Parser[NonEmptyList[Char]] = all.ws1
      def ws: Parser[List[Char]] = all.ws
    }

    implicit class AdditionalParserOps[T](parser: Parser[T]) {

      def ~~[K](n: => Parser[K]): Parser[(T, K)] =
        parser ~ ws1 ~ n -| { case ((t, _), k) => (t, k) }

      def manyWs1: Parser[NonEmptyList[T]] =
        parser.sepBy1(ws1)

      def manyWs: Parser[List[T]] =
        parser.sepBy(ws1)
    }
  }
}
