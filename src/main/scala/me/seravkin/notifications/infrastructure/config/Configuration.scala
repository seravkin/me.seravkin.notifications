package me.seravkin.notifications.infrastructure.config

import java.io.File
import java.nio.file.{Files, Paths}

import cats.effect._
import com.typesafe.config.ConfigFactory
import com.zaxxer.hikari.HikariConfig
import pureconfig._
import pureconfig.generic.ProductHint
import pureconfig.generic.auto._

object Configuration {

  final case class NotificationConfigurationRoot(notifications: NotificationConfiguration)
  final case class NotificationConfiguration(bot: BotConfiguration, database: HikariConfiguration)
  final case class JobConfiguration(interval: Int)
  final case class BotConfiguration(key: String, jobs: JobConfiguration)

  private[this] implicit def hint[T]: ProductHint[T] =
    ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))

  final case class HikariConfiguration(url: String, username: String, password: String,
                                       cachePrepStmts: Boolean, prepStmtCacheSize: Int, prepStmtCacheSqlLimit: Int) {

    def toHikariConfig: HikariConfig = {
      val config = new HikariConfig()

      config.setJdbcUrl(url)
      config.setUsername(username)
      config.setPassword(password)
      config.addDataSourceProperty("cachePrepStmts", cachePrepStmts.toString)
      config.addDataSourceProperty("prepStmtCacheSize", prepStmtCacheSize.toString)
      config.addDataSourceProperty("prepStmtCacheSqlLimit", prepStmtCacheSqlLimit.toInt)

      config
    }
  }

  def load(): IO[NotificationConfiguration] = IO {
    val configPath = Option("application.conf").filter(x => Files.exists(Paths.get(x)))
      .orElse(Option(System.getenv("NOTIFICATIONS_CONFIG_PATH")))

    val config = configPath
      .map(f => ConfigFactory.parseFile(new File(f)))
      .getOrElse(ConfigFactory.load())

    loadConfigOrThrow[NotificationConfigurationRoot](config).notifications
  }
}
