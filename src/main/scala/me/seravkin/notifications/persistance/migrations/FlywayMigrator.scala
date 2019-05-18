package me.seravkin.notifications.persistance.migrations

import cats.effect.Sync
import javax.sql.DataSource
import me.seravkin.notifications.persistance.Migrator
import org.flywaydb.core.Flyway

final class FlywayMigrator[F[_]: Sync] extends Migrator[F] {
  def migrate(source: DataSource): F[Unit] = Sync[F].delay {
    val flyway = Flyway.configure().dataSource(source).baselineOnMigrate(true).load()

    val _ = flyway.migrate()
  }
}
