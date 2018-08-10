package me.seravkin.notifications.infrastructure.state

import cats._
import cats.effect.Sync
import cats.implicits._

import scala.collection.concurrent.TrieMap

final class TrieChatStateRepository[S, F[_]: Sync](trie: TrieMap[Long, S], default: S) extends ChatStateRepository[S, F] {
  override def get(chatId: Long): F[S] =
    Sync[F].delay(trie.getOrElse(chatId, default))

  override def set(chatId: Long, s: S): F[Unit] =
    Sync[F].delay(trie += chatId -> s) >> Sync[F].unit

}
