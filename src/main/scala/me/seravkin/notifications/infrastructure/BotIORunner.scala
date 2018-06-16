package me.seravkin.notifications.infrastructure

import java.sql.Connection

import com.zaxxer.hikari.HikariDataSource
import doobie.free.KleisliInterpreter
import doobie.free.connection.unit
import doobie.util.transactor.{Strategy, Transactor}
import info.mukel.telegrambot4s.api.RequestHandler
import me.seravkin.notifications.bot.NotificationBot.ChatState
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.interpreters.BotIOInterpreter
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

sealed class BotIORunner(config: NotificationConfiguration, request: RequestHandler)  {

  private[this] val trieMap = new TrieMap[Long, ChatState]()
  private[this] val ds = new HikariDataSource(config.hikariConfig.toHikariConfig)

  def run(botIO: BotIO[Unit])(chatId: Option[Long]): Unit = {
    val connection: Connection = ds.getConnection

    connection.setAutoCommit(false)

    val transactor = Transactor(
      connection,
      Task.pure[Connection](_),
      KleisliInterpreter[Task].ConnectionInterpreter,
      Strategy.default.copy(always = unit, after = unit, oops = unit))

    botIO.foldMap(new BotIOInterpreter(chatId, trieMap, request, transactor)).runOnComplete {
      case Success(_) =>
        connection.commit()
        connection.close()
      case Failure(exception) =>
        exception.printStackTrace()
        connection.rollback()
        connection.close()
    }
  }

}
