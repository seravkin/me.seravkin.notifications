package me.seravkin.notifications

import java.sql.Connection
import java.util.concurrent.TimeUnit

import doobie.free.KleisliInterpreter
import doobie.free.connection.unit
import doobie.util.transactor.{Strategy, Transactor}
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.bot.NotificationBot.{ChatState, NotificationBotBuilder}
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.infrastructure.interpreters.BotIOInterpreter
import me.seravkin.notifications.infrastructure.{Bot, messages}
import me.seravkin.notifications.infrastructure.messages.BotIOSender
import me.seravkin.notifications.infrastructure.state.BotIOChatStateRepository
import me.seravkin.notifications.infrastructure.time.ActualSystemDateTime
import me.seravkin.notifications.persistance.botio.{DoobieNotificationTasksRepository, DoobieNotificationsRepository, DoobieUsersRepository}
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.domain.services.NotificationTasksServiceImpl
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

object Main extends App with TelegramBot with Polling {

  private[this] val trieMap = new TrieMap[Long, ChatState]()

  implicit def messageForMessage: infrastructure.messages.Message[Message] = new messages.Message[Message] {
    override def data(msg: Message): Option[String] =
      None

    override def chatId(msg: Message): Long =
      msg.chat.id

    override def text(msg: Message): Option[String] =
      msg.text

    override def username(msg: Message): Option[String] =
      msg.chat.username

    override def isPrivate(msg: Message): Boolean =
      msg.chat.`type` == ChatType.Private
  }

  implicit def messageForCallbackQuery: infrastructure.messages.Message[CallbackQuery] = new messages.Message[CallbackQuery] {
    override def data(msg: CallbackQuery): Option[String] =
      msg.data

    override def chatId(msg: CallbackQuery): Long =
      msg.message.get.chat.id

    override def text(msg: CallbackQuery): Option[String] =
      None

    override def username(msg: CallbackQuery): Option[String] =
      msg.from.username

    override def isPrivate(msg: CallbackQuery): Boolean =
      msg.message.get.chat.`type` == ChatType.Private
  }

  private[this] def botFor[Msg: infrastructure.messages.Message] =
    NotificationBotBuilder[Msg, BotIO](
      DoobieUsersRepository,
      BotIOChatStateRepository,
      BotIOSender,
      CombinatorMomentInFutureParser,
      DoobieNotificationsRepository,
      ActualSystemDateTime).build

  private[this] val service = NotificationTasksServiceImpl(
    ActualSystemDateTime,
    DoobieNotificationTasksRepository,
    DoobieNotificationsRepository,
    BotIOSender)

  private[this] val messageBot = botFor[Message]
  private[this] val callbackQueryBot = botFor[CallbackQuery]

  private[this] val config = new HikariConfig
  config.setJdbcUrl("jdbc:postgresql://localhost:5432/notification_bot")
  config.setUsername("postgres")
  config.setPassword("")
  config.addDataSourceProperty("cachePrepStmts", "true")
  config.addDataSourceProperty("prepStmtCacheSize", "250")
  config.addDataSourceProperty("prepStmtCacheSqlLimit", "2048")

  private[this] val ds = new HikariDataSource(config)

  private[this] def interpret(botIO: BotIO[Unit])(chatId: Option[Long]) = {
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

  private[this] def interpret[T: infrastructure.messages.Message](bot: Bot[T, BotIO])(message: T, chatId: Long): Unit =
    interpret(bot(message))(Some(chatId))

  def token = ""

  override def receiveMessage(message: Message): Unit =
    interpret(messageBot)(message, message.chat.id)

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): Unit = {
    if(callbackQuery.message.nonEmpty) {
      interpret(callbackQueryBot)(callbackQuery, callbackQuery.message.get.chat.id)
    }
  }

  global.scheduleWithFixedDelay(10, 10, TimeUnit.SECONDS, () => {
    interpret(service.sendNotificationsIfNeeded())(None)
  })

  this.run()

}
