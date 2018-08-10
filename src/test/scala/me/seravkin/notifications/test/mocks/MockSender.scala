package me.seravkin.notifications.test.mocks

import cats.data.{State, StateT}
import cats._
import cats.syntax.all._
import info.mukel.telegrambot4s.models.Message
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.test.mocks.MockBotState._

final class MockSender[F[_]: Monad] extends Sender[StateT[F, MockBotState, ?]] {
  override def ask(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None):
    StateT[F, MockBotState, Int] =
    idToEdit match {
      case Some(id) => for(
        list  <- StateT.inspect[F, MockBotState, List[MockMessage]](_.sentMessages);
        elem  = list.find(_.id == id).get;
        index = list.indexWhere(_.id == id);
        _     <- StateT.modify[F, MockBotState](messages.set(_)(list.updated(index, elem.copy(text = text, buttons = buttonWithCommand))))
      ) yield id

      case None => for(
        list  <- StateT.inspect[F, MockBotState, List[MockMessage]](_.sentMessages);
        newId =  list.length + 1;
        _     <- StateT.modify[F, MockBotState](messages.modify(_)(list => list :+ MockMessage(newId,PersistedUser(1, Some(chatId), ""), text, buttons = buttonWithCommand)))
      ) yield newId
    }

  override def tell(chatId: Long, text: String, buttonWithCommand: List[Button] = Nil, idToEdit: Option[Int] = None):
    StateT[F, MockBotState, Unit] =
    ask(chatId, text, buttonWithCommand, idToEdit).map(_ => ())

}
