package me.seravkin.notifications

import java.io.File
import java.nio.file.{Files, Path, Paths}
import java.sql.Connection
import java.util.concurrent.TimeUnit

import cats._
import cats.data._
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp, Sync}
import com.typesafe.config.ConfigFactory
import doobie.free.KleisliInterpreter
import doobie.free.connection.unit
import doobie.util.transactor.{Strategy, Transactor}
import info.mukel.telegrambot4s.api.{Polling, RequestHandler, TelegramBot}
import info.mukel.telegrambot4s.models.{CallbackQuery, ChatType, Message}
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.infrastructure.interpreters.ReaderInterpreter
import me.seravkin.notifications.infrastructure.messages.RequestHandlerSender
import me.seravkin.notifications.infrastructure.state.{ChatStateRepository, TrieChatStateRepository}
import me.seravkin.notifications.infrastructure.time.ActualSystemDateTime
import me.seravkin.notifications.persistance.botio.{DoobieNotificationTasksRepository, DoobieNotificationsRepository, DoobieUsersRepository}
import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.bot.services.{NotificationChatServiceImpl, PageViewImpl, TimeBeautifyServiceImpl}
import me.seravkin.notifications.bot.{ChatState, Nop, NotificationBot}
import me.seravkin.notifications.domain.interpreter.DatesAst
import me.seravkin.notifications.domain.services.NotificationTasksServiceImpl
import me.seravkin.notifications.infrastructure.BotF
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.random.SyncRandom
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import me.seravkin.tg.adapter.events.BotEvent
import me.seravkin.tg.adapter.requests.{RequestHandlerAdapter, RequestHandlerF}
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

  private[this] def datesAst[F[_]: Sync]: DatesAst[F] =
    new DatesAst[F](new SyncRandom[F]())

  private[this] def parser[F[_]: Sync] =
    new CombinatorMomentInFutureParser(datesAst[F], datesAst[F])

  private[this] def botFor[F[_]: Sync](chatStateRepository: ChatStateRepository[ChatState, BotF[F, ?]],
                                        req: RequestHandlerF[BotF[F, ?]]): NotificationBot[BotF[F, ?]] = NotificationBot[BotF[F, ?]](
    new DoobieUsersRepository[F],
    chatStateRepository,
    new RequestHandlerSender[BotF[F, ?]](req),
    parser[BotF[F, ?]],
    new DoobieNotificationsRepository,
    new NotificationChatServiceImpl(
      new DoobieNotificationsRepository[F],
      new DoobieUsersRepository[F],
      chatStateRepository,
      parser[BotF[F, ?]],
      ActualSystemDateTime,
      new TimeBeautifyServiceImpl(ActualSystemDateTime),
      new RequestHandlerSender[BotF[F, ?]](req)),
    new PageViewImpl(
      new DoobieNotificationsRepository[F],
      new RequestHandlerSender[BotF[F, ?]](req)
    ),
    ActualSystemDateTime)


  private[this] def create(config: NotificationConfiguration,
                           source: HikariDataSource,
                           requestHandler: RequestHandler): IO[Bot[IO]] = {
    val adapter = new RequestHandlerAdapter[BotF[IO, ?]](requestHandler)
    val map = new TrieMap[Long, ChatState]()

    val service = NotificationTasksServiceImpl(
      ActualSystemDateTime,
      new DoobieNotificationTasksRepository[IO],
      new DoobieNotificationsRepository[IO],
      new RequestHandlerSender[BotF[IO, ?]](adapter))

    val bot = botFor(new TrieChatStateRepository[ChatState, BotF[IO, ?]](map, Nop), adapter)

    val interpreterK = new ReaderInterpreter(source.getConnection)

    IO {
      global.scheduleWithFixedDelay(config.secondsForScheduler, config.secondsForScheduler, TimeUnit.SECONDS, () => {
        interpreterK(service.sendNotificationsIfNeeded()).unsafeRunSync()
      })

      Kleisli(bot)
        .mapK(interpreterK)
    }
  }

  private[this] def dataSource(config: NotificationConfiguration) : IO[HikariDataSource] = IO {
    new HikariDataSource(config.hikariConfig.toHikariConfig)
  }



}
