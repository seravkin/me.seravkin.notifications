package me.seravkin.notifications.test.mocks

import cats.data.State
import cats._
import cats.syntax.all._
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.test.mocks.MockBotState._

object MockSender extends Sender[State[MockBotState, ?]] {
  override def send(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None): State[MockBotState, Int] =
    idToEdit match {
      case Some(id) => for(
        list  <- State.inspect[MockBotState, List[MockMessage]](_.sentMessages);
        elem  = list.find(_.id == id).get;
        index = list.indexWhere(_.id == id);
        _     <- State.modify[MockBotState](messages.set(_)(list.updated(index, elem.copy(text = text, buttons = buttonWithCommand))))
      ) yield id

      case None => for(
        list  <- State.inspect[MockBotState, List[MockMessage]](_.sentMessages);
        newId =  list.length + 1;
        _     <- State.modify[MockBotState](messages.modify(_)(list => list :+ MockMessage(newId,User(1, Some(chatId), ""), text, buttons = buttonWithCommand)))
      ) yield newId
    }

}
