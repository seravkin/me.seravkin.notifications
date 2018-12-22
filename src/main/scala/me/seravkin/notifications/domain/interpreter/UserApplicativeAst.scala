package me.seravkin.notifications.domain.interpreter

import cats.Applicative
import me.seravkin.notifications.domain.ast.UserAst

final class UserApplicativeAst[F[_]: Applicative: DateProvider] extends UserAst[F[Dates]] {
  override def forUser(username: String, t: F[Dates]): F[Dates] = t
}
