package me.seravkin.notifications

import java.time.LocalDateTime
import java.util.concurrent.TimeUnit

import cats.data.{EitherT, Kleisli, ReaderT}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.mtl.implicits._
import cats.syntax.applicativeError._
import cats.effect.{Concurrent, Sync, Timer}
import cats.{Defer, MonadError, ~>}
import com.bot4s.telegram.api.RequestHandler
import doobie.util.transactor.Transactor
import me.seravkin.notifications.bot.ChatState.Nop
import me.seravkin.notifications.bot.services.{NotificationChatServiceImpl, PageViewImpl, TimeBeautifyServiceImpl}
import me.seravkin.notifications.bot.{ChatState, NotificationBot}
import me.seravkin.notifications.domain.interpreter._
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

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.FiniteDuration

final class Wiring[F[_]: Concurrent: Timer] {


  private[this] def parser[G[_]: DateProvider: MonadError[?[_], String]: Defer] =
    new CombinatorMomentInFutureParser(
      new DurationApplicativeAst[G](),
      new TimeApplicativeAst[G](new SyncRandom[G]),
      new DateApplicativeAst[G](),
      new DateAndTimeApplicativeAst[G](),
      new RelativeApplicativeAst[G](),
      new UserApplicativeAst[G](),
      new ConfirmationApplicativeAst[G](),
      new RecurrentApplicativeAst[G]()
    )

  private[this] implicit def deferForFactory[G[_]: Sync]: Defer[DatesFactory[BotF[G, ?], ?]] =
    new Defer[DatesFactory[BotF[G, ?], ?]] {
      override def defer[A](fa: => DatesFactory[BotF[G, ?], A]): DatesFactory[BotF[G, ?], A] =
        ReaderT[EitherT[BotF[G, ?], String,?], LocalDateTime, A](n =>
          EitherT(
            ReaderT[G, Transactor[G], Either[String, A]](tx =>
              Sync[G].suspend(fa(n).value(tx))
            )
          )
        )
    }


  private[this] def botFor(chatStateRepository: ChatStateRepository[ChatState, BotF[F, ?]],
                           req: RequestHandlerF[BotF[F, ?]]): NotificationBot[BotF[F, ?]] =
    NotificationBot[BotF[F, ?]](
      new DoobieUsersRepository[F],
      chatStateRepository,
      new RequestHandlerSender[BotF[F, ?]](req),
      parser[DatesFactory[BotF[F, ?], ?]],
      new DoobieNotificationsRepository,
      new NotificationChatServiceImpl(
        new DoobieNotificationsRepository[F],
        new DoobieUsersRepository[F],
        chatStateRepository,
        parser[DatesFactory[BotF[F, ?], ?]],
        new ActualSystemDateTime,
        new TimeBeautifyServiceImpl(new ActualSystemDateTime),
        new RequestHandlerSender[BotF[F, ?]](req)),
      new PageViewImpl(
        new DoobieNotificationsRepository[F],
        new RequestHandlerSender[BotF[F, ?]](req)
      ),
      new ActualSystemDateTime)

  def create(config: NotificationConfiguration,
             interpreterK: BotF[F, ?] ~> F,
             requestHandler: RequestHandler): F[Bot[F]] = {

    val adapter = new RequestHandlerAdapter[BotF[F, ?]](requestHandler)
    val map = new TrieMap[Long, ChatState]()

    val service = NotificationTasksServiceImpl(
      new ActualSystemDateTime[BotF[F, ?]],
      new DoobieNotificationsRepository[F],
      new RequestHandlerSender[BotF[F, ?]](adapter))

    val bot = botFor(new TrieChatStateRepository[ChatState, BotF[F, ?]](map, Nop), adapter)

    def schedule: F[Unit] =
      Timer[F].sleep(FiniteDuration(config.bot.jobs.interval.toLong,TimeUnit.SECONDS)) >>
      interpreterK(service.sendNotificationsIfNeeded()).attempt >>
      Sync[F].suspend(schedule)

    Concurrent[F].start(schedule).map(_ =>
      Kleisli(bot)
        .mapK(interpreterK)
    )
  }
}
