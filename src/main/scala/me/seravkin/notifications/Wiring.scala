package me.seravkin.notifications

import java.util.concurrent.TimeUnit

import cats.data.Kleisli
import cats.effect.{Concurrent, Sync}
import cats.{Id, ~>}
import com.zaxxer.hikari.HikariDataSource
import info.mukel.telegrambot4s.api.RequestHandler
import me.seravkin.notifications.bot.ChatState.Nop
import me.seravkin.notifications.bot.services.{NotificationChatServiceImpl, PageViewImpl, TimeBeautifyServiceImpl}
import me.seravkin.notifications.bot.{ChatState, NotificationBot}
import me.seravkin.notifications.domain.interpreter.DatesAst
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.domain.services.NotificationTasksServiceImpl
import me.seravkin.notifications.infrastructure.BotF
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.messages.RequestHandlerSender
import me.seravkin.notifications.infrastructure.random.SyncRandom
import me.seravkin.notifications.infrastructure.state.{ChatStateRepository, TrieChatStateRepository}
import me.seravkin.notifications.infrastructure.time.ActualSystemDateTime
import me.seravkin.notifications.persistance.botio.{DoobieNotificationsRepository, DoobieUsersRepository}
import me.seravkin.tg.adapter.Bot
import me.seravkin.tg.adapter.requests.{RequestHandlerAdapter, RequestHandlerF}
import monix.execution.Scheduler

import scala.collection.concurrent.TrieMap

class Wiring[F[_]: Concurrent] {

  private[this] def datesAst[G[_]: Sync]: DatesAst[G] =
    new DatesAst[G](new SyncRandom[G]())

  private[this] def parser[G[_]: Sync] =
    new CombinatorMomentInFutureParser(datesAst[G], datesAst[G])

  private[this] def botFor(chatStateRepository: ChatStateRepository[ChatState, BotF[F, ?]],
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

  def create(config: NotificationConfiguration,
             source: HikariDataSource,
             scheduler: Scheduler,
             interpreterK: BotF[F, ?] ~> F,
             toUnitK: F ~> Id,
             requestHandler: RequestHandler): F[Bot[F]] = {
    val adapter = new RequestHandlerAdapter[BotF[F, ?]](requestHandler)
    val map = new TrieMap[Long, ChatState]()

    val service = NotificationTasksServiceImpl(
      ActualSystemDateTime,
      new DoobieNotificationsRepository[F],
      new RequestHandlerSender[BotF[F, ?]](adapter))

    val bot = botFor(new TrieChatStateRepository[ChatState, BotF[F, ?]](map, Nop), adapter)

    Sync[F].delay {
      scheduler.scheduleWithFixedDelay(config.secondsForScheduler.toLong, config.secondsForScheduler.toLong, TimeUnit.SECONDS, () => {
        interpreterK.andThen(toUnitK).apply(service.sendNotificationsIfNeeded())
      })

      Kleisli(bot)
        .mapK(interpreterK)
    }

  }
}
