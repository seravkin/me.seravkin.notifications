package me.seravkin.notifications

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.sql.Connection
import java.util.concurrent.TimeUnit

import cats._
import cats.data._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.config.ConfigFactory
import doobie.free.KleisliInterpreter
import doobie.free.connection.unit
import doobie.util.transactor.{Strategy, Transactor}
import info.mukel.telegrambot4s.api.{Polling, RequestHandler, TelegramBot}
import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.domain.algebra.BotAlgebra.BotIO
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.infrastructure.interpreters.{BotIOInterpreterK, BotOpInterpreter, ReaderInterpreter}
import me.seravkin.notifications.infrastructure.messages.BotIOSender
import me.seravkin.notifications.infrastructure.state.BotIOChatStateRepository
import me.seravkin.notifications.infrastructure.time.ActualSystemDateTime
import me.seravkin.notifications.persistance.botio.{DoobieNotificationTasksRepository, DoobieNotificationsRepository, DoobieUsersRepository}
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.bot.services.{NotificationChatServiceImpl, PageViewImpl}
import me.seravkin.notifications.bot.{ChatState, NotificationBot}
import me.seravkin.notifications.domain.services.NotificationTasksServiceImpl
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import me.seravkin.tg.adapter.events.BotEvent
import me.seravkin.tg.adapter.requests.RequestHandlerAdapter
import me.seravkin.tg.adapter.{Bot, TelegramBotAdapter}

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for(
    config  <- Configuration.load();
    source  <- dataSource(config);
    adapter = new TelegramBotAdapter(config.telegramApiKey, create(config, source, _));
    _       <- adapter.runSafe()
  ) yield ExitCode.Success

  private[this] def botFor: NotificationBot[BotIO] = NotificationBot[BotIO](DoobieUsersRepository,
    BotIOChatStateRepository,
    BotIOSender,
    CombinatorMomentInFutureParser,
    DoobieNotificationsRepository,
    new NotificationChatServiceImpl(DoobieNotificationsRepository,
      DoobieUsersRepository,
      BotIOChatStateRepository,
      CombinatorMomentInFutureParser,
      ActualSystemDateTime,
      BotIOSender),
    new PageViewImpl(
      DoobieNotificationsRepository,
      BotIOSender
    ),
    ActualSystemDateTime)


  private[this] def create(config: NotificationConfiguration,
                           source: HikariDataSource,
                           requestHandler: RequestHandler): IO[Bot[IO]] = {
    val service = NotificationTasksServiceImpl(
      ActualSystemDateTime,
      DoobieNotificationTasksRepository,
      DoobieNotificationsRepository,
      BotIOSender)
    val map = new TrieMap[Long, ChatState]()

    val interpreterK =
      BotIOInterpreterK(
        BotOpInterpreter(map,
          new RequestHandlerAdapter[IO](requestHandler)))
          .andThen[IO](new ReaderInterpreter(source.getConnection))

    IO {
      global.scheduleWithFixedDelay(config.secondsForScheduler, config.secondsForScheduler, TimeUnit.SECONDS, () => {
        interpreterK(service.sendNotificationsIfNeeded()).unsafeRunSync()
      })
    } >>
    IO {
      Kleisli(botFor)
        .mapK(interpreterK)
    }

  }

  private[this] def dataSource(config: NotificationConfiguration) : IO[HikariDataSource] = IO {
    new HikariDataSource(config.hikariConfig.toHikariConfig)
  }



}
