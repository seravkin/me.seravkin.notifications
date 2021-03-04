package me.seravkin.notifications

import java.util.concurrent.Executors

import cats.effect.{ExitCode, IO, IOApp}
import com.bot4s.telegram.api.Polling
import com.bot4s.telegram.clients.ScalajHttpClient
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.interpreters.ReaderInterpreter
import me.seravkin.notifications.persistance.migrations.FlywayMigrator
import me.seravkin.tg.adapter.TelegramBotAdapter

import scala.concurrent.ExecutionContext

object Main extends IOApp {

  private[this] implicit val blockingEc: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  override def run(args: List[String]): IO[ExitCode] = for (
    config   <- Configuration.load();
    source   <- dataSource(config);
    migrator =  new FlywayMigrator[IO];
    _        <- migrator.migrate(source);
    adapter = new TelegramBotAdapter(
      new ScalajHttpClient(config.bot.key),
      new Wiring[IO].create(config, new ReaderInterpreter(blockingEc, source.getConnection), _)) with Polling;
    _        <- adapter.runSafe()
  ) yield ExitCode.Success


  private[this] def dataSource(config: NotificationConfiguration): IO[HikariDataSource] = IO {
    new HikariDataSource(config.database.toHikariConfig)
  }


}
