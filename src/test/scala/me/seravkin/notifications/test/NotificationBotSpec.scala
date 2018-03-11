package me.seravkin.notifications.test

import java.time.LocalDateTime

import cats._
import cats.data._
import cats.syntax.all._
import me.seravkin.notifications.bot.NotificationBot
import me.seravkin.notifications.bot.NotificationBot.{ChatState, Nop, NotificationBotBuilder}
import me.seravkin.notifications.domain.Notifications.{Notification, OneTime, Recurrent}
import me.seravkin.notifications.domain.User
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.infrastructure.Bot
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.ActualSystemDateTime
import me.seravkin.notifications.persistance.NotificationsRepository
import org.scalatest._
import shapeless._
import me.seravkin.notifications.test.mocks._

class NotificationBotSpec extends FlatSpec with Matchers {

  "Notifications bot" should "show help when help command is sent" in {
    val dialogue = for(
      _ <- send("/help");
      _ <- shouldAnswerWith(helpText)
    ) yield ()

    dialogue.run(MockBotState(defaultUser :: Nil)).value
  }

  it should "show help on start command too" in {
    val dialogue = for(
      _ <- send("/start");
      _ <- shouldAnswerWith(helpText)
    ) yield ()

    dialogue.run(MockBotState(defaultUser :: Nil)).value
  }

  it should "show active notifications for current user if they are present when show command is sent" in {
    val dialogue = for(
      _ <- send("/show");
      _ <- shouldAnswerWith("Напоминания:\n\n" +
        "Напоминание 1 о \"test 1\"\n" +
        "Напоминание 3 о \"rec test 1\"")
    ) yield ()


    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .value
  }

  it should "deny access for not authorized user" in {
    val dialogue = for(
      _ <- send("/help", User(2, Some(2), "2"));
      _ <- shouldAnswerWith("Пользователь не аутентифицирован для данного сервиса")
    ) yield ()

    dialogue.run(MockBotState(users = defaultUser :: Nil)).value
  }

  it should "make active notification for current user unactive when delete command is sent if id is correct" in {

    val dialogue = for(
      _ <- send("/delete 1");
      _ <- shouldAnswerWith("Напоминание удалено")
    ) yield ()

    val (state, _) = dialogue
        .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
        .value

    val changedNotification = state.notifications.find(_.id == 1)

    changedNotification.nonEmpty should be (true)
    changedNotification.get.isActive should be (false)
  }

  it should "parse notification request and store notification if request is correct" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith("Введите напоминание:");
      _  <- send("test 1");
      _  <- shouldAnswerWith("Введите желаемое время:");
      _  <- send("завтра в 12:35");
      _  <- shouldAnswerWith("Напоминание поставлено и будет отправлено 23.08 в 12:35")
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).value

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "try asking user again if request is incorrect and request is multiline" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith("Введите напоминание:");
      _  <- send("test 1");
      _  <- shouldAnswerWith("Введите желаемое время:");
      _  <- send("завтра в 42:35");
      _  <- shouldAnswerWith(_.startsWith("Время в неправильном формате"));
      _  <- send("завтра в 0:32");
      _  <- shouldAnswerWith("Напоминание поставлено и будет отправлено 23.08 в 00:32")
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).value

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "allow user to exit from dialogue before time input" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith("Введите напоминание:");
      _  <- send("/exit");
      _  <- shouldAnswerWith("Создание напоминания отменено")
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).value

    state.notifications.length should be (0)
  }

  it should "parse notification request and store notification if request is correct and in one line" in {
    val dialogue = for(
      _  <- send("/in \"test 1\" сегодня в 8:49");
      _  <- shouldAnswerWith("Напоминание поставлено и будет отправлено в 08:49")
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).value

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "work correctly in longer dialogue with creating showing and deleting notifications" in {
    val dialogue = for(
      _ <- send("/in \"test 1\" 22.08 в 23:55");
      _ <- shouldAnswerWith("Напоминание поставлено и будет отправлено в 23:55");
      _ <- send("/in \"test 2\" через 4 дня в это же время");
      _ <- shouldAnswerWith("Напоминание поставлено и будет отправлено 26.08 в 12:00");
      _ <- send("/show");
      _ <- shouldAnswerWith(t => t.contains("test 1") && t.contains("test 2"));
      _ <- send("/in");
      _ <- send("test 3");
      _ <- send("послезавтра в 9:55");
      _ <- send("/delete 1");
      _ <- shouldAnswerWith("Напоминание удалено");
      _ <- send("/show");
      _ <- shouldAnswerWith(t => t.contains("test 2") && t.contains("test 3"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).value

    state.notifications.length should be (3)
  }


  private[this] def send(text: String, user: User = defaultUser) =
    bot(MockMessage(user, text))

  private[this] def shouldAnswerWith(text: String) = State.get[MockBotState] map { state =>
    val lastMessage = state.sentMessages.last

    lastMessage.text should be (text)
  }

  private[this] def shouldAnswerWith(f: String => Boolean) = State.get[MockBotState] map { state =>
    val lastMessage = state.sentMessages.last

    f(lastMessage.text) should be (true)
  }

  private[this] val defaultUser = User(1, Some(1), "test")
  private[this] val defaultUserId = defaultUser.id
  private[this] val mockedDateTime = MockDateTime(LocalDateTime.of(2018, 8, 22, 12, 0, 0))

  private[this] val helpText = "Бот c напоминаниями\n" +
    "/in - Напоминает о событии через заданный интервал времени\n" +
    "/show - Показывает активные напоминания\n" +
    "/delete <id> - Удаляет напоминания с указанным id"

  private[this] val existingNotifications =  Vector(
    OneTime(1, defaultUserId, "test 1", LocalDateTime.now(), true),
    OneTime(2, defaultUserId, "test 1 na", LocalDateTime.now(), false),
    Recurrent(3, defaultUserId, "rec test 1", LocalDateTime.now(), LocalDateTime.now(), true),
    Recurrent(4, defaultUserId, "rec test 1 - na", LocalDateTime.now(), LocalDateTime.now(), false),
    Recurrent(5, defaultUserId + 1, "asdasd", LocalDateTime.now(), LocalDateTime.now(), true)
  )

  private[this] val bot: Bot[MockMessage, State[MockBotState, ?]] =
    NotificationBotBuilder[MockMessage, State[MockBotState, ?]](
      MockUsersRepository(defaultUser),
      MockChatStateRepository,
      MockSender,
      CombinatorMomentInFutureParser,
      MockNotificationRepository,
      mockedDateTime).build

}
