package me.seravkin.notifications

import java.util.concurrent.Executors

import cats.effect.{ExitCode, IO, IOApp}
import com.bot4s.telegram.api.Polling
import com.bot4s.telegram.clients.ScalajHttpClient
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.interpreters.{ReaderInterpreter, UnsafeIOInterpreter}
import me.seravkin.tg.adapter.TelegramBotAdapter

import scala.concurrent.ExecutionContext

object Main extends IOApp {

  private[this] implicit val blockingEc: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  override def run(args: List[String]): IO[ExitCode] = for (
    config <- Configuration.load();
    source <- dataSource(config);
    adapter = new TelegramBotAdapter(
      new ScalajHttpClient(config.telegramApiKey),
      new Wiring[IO].create(config,
        source,
        new ReaderInterpreter(blockingEc, source.getConnection),
        UnsafeIOInterpreter, _)
    ) with Polling;
    _ <- adapter.runSafe()
  ) yield ExitCode.Success


  private[this] def dataSource(config: NotificationConfiguration): IO[HikariDataSource] = IO {
    new HikariDataSource(config.hikariConfig.toHikariConfig)
  }


}
