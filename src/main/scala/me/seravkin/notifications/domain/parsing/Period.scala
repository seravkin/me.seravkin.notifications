package me.seravkin.notifications.domain.parsing

sealed trait Period { def period: (Int, Int); }

object Period {

  case object Night extends Period { override def period: (Int, Int) = 0 -> 8 }

  case object Morning extends Period { override def period: (Int, Int) = 8 -> 12 }

  case object DayAsTime extends Period { override def period: (Int, Int) = 12 -> 19 }

  case object Evening extends Period { override def period: (Int, Int) = 19 -> 23 }

}

