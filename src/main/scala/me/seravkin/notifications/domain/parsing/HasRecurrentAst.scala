package me.seravkin.notifications.domain.parsing

trait HasRecurrentAst[R] {
  def recurrentAst: RecurrentAst[R]
}
