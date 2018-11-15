package me.seravkin.notifications.infrastructure.config

import java.io.File
import java.nio.file.{Files, Paths}

import cats.effect._
import com.typesafe.config.ConfigFactory
import com.zaxxer.hikari.HikariConfig

object Configuration {

  case class HikariConfiguration(jdbcUrl: String, username: String, password: String,
                                 cachePrepStmts: Boolean, prepStmtCacheSize: Int, prepStmtCacheSqlLimit: Int) {

    def toHikariConfig: HikariConfig = {
      val config = new HikariConfig()

      config.setJdbcUrl(jdbcUrl)
      config.setUsername(username)
      config.setPassword(password)
      config.addDataSourceProperty("cachePrepStmts", cachePrepStmts.toString)
      config.addDataSourceProperty("prepStmtCacheSize", prepStmtCacheSize.toString)
      config.addDataSourceProperty("prepStmtCacheSqlLimit", prepStmtCacheSqlLimit.toInt)

      config
    }
  }

  case class NotificationConfiguration(telegramApiKey: String, secondsForScheduler: Int, hikariConfig: HikariConfiguration)

  def load(): IO[NotificationConfiguration] = IO {
    val configPath = Option("application.conf").filter(x => Files.exists(Paths.get(x)))
      .orElse(Option(System.getenv("NOTIFICATIONS_CONFIG_PATH")))

    val config = configPath
      .map(f => ConfigFactory.parseFile(new File(f)))
      .getOrElse(ConfigFactory.load())

    NotificationConfiguration(
      config.getString("notifications.bot.key"),
      config.getInt("notifications.bot.jobs.interval"),
      HikariConfiguration(
        config.getString("notifications.database.url"),
        config.getString("notifications.database.username"),
        config.getString("notifications.database.password"),
        config.getBoolean("notifications.database.cachePrepStmts"),
        config.getInt("notifications.database.prepStmtCacheSize"),
        config.getInt("notifications.database.prepStmtCacheSqlLimit")
      )
    )
  }
}
