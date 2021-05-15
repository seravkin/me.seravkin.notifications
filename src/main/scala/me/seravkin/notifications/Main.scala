package me.seravkin.notifications

import java.util.concurrent.Executors
import cats.effect.{ExitCode, IO, IOApp}
import com.zaxxer.hikari.HikariDataSource
import me.seravkin.notifications.infrastructure.config.Configuration
import me.seravkin.notifications.infrastructure.config.Configuration.NotificationConfiguration
import me.seravkin.notifications.infrastructure.interpreters.ReaderInterpreter
import me.seravkin.notifications.infrastructure.telegram.TelegramBotAdapter
import me.seravkin.notifications.persistance.migrations.FlywayMigrator

import scala.concurrent.ExecutionContext

object Main extends IOApp {

  private[this] implicit val blockingEc: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  override def run(args: List[String]): IO[ExitCode] = for (
    config   <- Configuration.load();
    source   <- dataSource(config);
    migrator =  new FlywayMigrator[IO];
    _        <- migrator.migrate(source);
    adapter = new TelegramBotAdapter(
      config.bot.key,
      new Wiring[IO].create(config, new ReaderInterpreter(blockingEc, source.getConnection), _));
    _        <- adapter.run()
  ) yield ExitCode.Success


  private[this] def dataSource(config: NotificationConfiguration): IO[HikariDataSource] = IO {
    new HikariDataSource(config.database.toHikariConfig)
  }


}
