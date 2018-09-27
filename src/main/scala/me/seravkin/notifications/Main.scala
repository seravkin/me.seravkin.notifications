package me.seravkin.notifications

import cats.effect.{ExitCode, IO, IOApp}
import com.zaxxer.hikari.HikariDataSource
import info.mukel.telegrambot4s.api.Polling
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.interpreters.{ReaderInterpreter, UnsafeIOInterpreter}
import me.seravkin.tg.adapter.TelegramBotAdapter
import monix.execution.Scheduler.Implicits.global

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for (
    config <- Configuration.load();
    source <- dataSource(config);
    adapter = new TelegramBotAdapter(
      config.telegramApiKey,
      new Wiring[IO].create(config,
        source,
        global,
        new ReaderInterpreter(source.getConnection),
        UnsafeIOInterpreter, _)
    ) with Polling;
    _ <- adapter.runSafe()
  ) yield ExitCode.Success


  private[this] def dataSource(config: NotificationConfiguration): IO[HikariDataSource] = IO {
    new HikariDataSource(config.hikariConfig.toHikariConfig)
  }


}
