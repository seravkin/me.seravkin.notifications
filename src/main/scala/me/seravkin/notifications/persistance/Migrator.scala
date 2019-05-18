package me.seravkin.notifications.persistance

import javax.sql.DataSource

trait Migrator[F[_]] {
  def migrate(dataSource: DataSource): F[Unit]
}
