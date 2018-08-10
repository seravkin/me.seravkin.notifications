package me.seravkin.notifications.test

import java.time.LocalDateTime

import cats._
import cats.data._
import cats.effect.IO
import cats.syntax.all._
import info.mukel.telegrambot4s.methods.SendMessage
import info.mukel.telegrambot4s.models._
import me.seravkin.notifications.bot.NotificationBot
import me.seravkin.notifications.bot._
import me.seravkin.notifications.bot.services.{NotificationChatServiceImpl, PageViewImpl, TimeBeautifyServiceImpl}
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.interpreter._
import me.seravkin.notifications.domain.parsing.CombinatorMomentInFutureParser
import me.seravkin.notifications.infrastructure.messages.{Button, Sender}
import me.seravkin.notifications.infrastructure.random.Random
import me.seravkin.notifications.infrastructure.state.ChatStateRepository
import me.seravkin.notifications.infrastructure.time.{ActualSystemDateTime, SystemDateTime}
import me.seravkin.notifications.persistance.NotificationsRepository
import org.scalatest._
import shapeless._
import me.seravkin.notifications.test.mocks._
import me.seravkin.tg.adapter.Bot
import me.seravkin.tg.adapter.events._

import scala.util

class NotificationBotSpec extends FlatSpec with Matchers {

  "Notifications bot" should "show help when help command is sent" in {
    val dialogue = for(
      _ <- send("/help");
      _ <- shouldAnswerWith(sentMessage)(hasExpected(helpText))
    ) yield ()

    dialogue.run(MockBotState(defaultUser :: Nil)).unsafeRunSync()
  }

  it should "show help on start command too" in {
    val dialogue = for(
      _ <- send("/start");
      _ <- shouldAnswerWith(sentMessage)(hasExpected(helpText))
    ) yield ()

    dialogue.run(MockBotState(defaultUser :: Nil)).unsafeRunSync()
  }

  it should "show active notifications for current user if they are present when show command is sent" in {
    val dialogue = for(
      _ <- send("/show");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминания:\n\n" +
        "Напоминание 1 о \"test 1\" в 2018-12-01T12:00\n" +
        "Напоминание 3 о \"rec test 1\""))
    ) yield ()


    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "deny access for not authorized user" in {
    val dialogue = for(
      _ <- send("/help", User(2, false,"2", username = Some("2")));
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Пользователь не аутентифицирован для данного сервиса"))
    ) yield ()

    dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()
  }

  it should "make active notification for current user unactive when delete command is sent if id is correct" in {

    val dialogue = for(
      _ <- send("/delete 1");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание удалено"))
    ) yield ()

    val (state, _) = dialogue
        .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
        .unsafeRunSync()

    val changedNotification = state.notifications.find(_.id == 1)

    changedNotification.nonEmpty should be (true)
    changedNotification.get.isActive should be (false)
  }

  it should "parse notification request and store notification if request is correct" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Введите напоминание:"));
      _  <- send("test 1");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Введите желаемое время:"));
      _  <- send("завтра в 12:35");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено 23.08 в 12:35"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "try asking user again if request is incorrect and request is multiline" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Введите напоминание:"));
      _  <- send("test 1");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Введите желаемое время:"));
      _  <- send("завтра в 42:35");
      _  <- shouldAnswerWithTextThat(_.startsWith("Время в неправильном формате"));
      _  <- send("завтра в 0:32");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено 23.08 в 00:32"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "allow user to exit from dialogue before time input" in {
    val dialogue = for(
      _  <- send("/in");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Введите напоминание:"));
      _  <- send("/exit");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Создание напоминания отменено"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()

    state.notifications.length should be (0)
  }

  it should "parse notification request and store notification if request is correct and in one line" in {
    val dialogue = for(
      _  <- send("/in \"test 1\" сегодня в 8:49");
      _  <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено в 08:49"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()

    state.notifications.length should be (1)

    val notification = state.notifications.head

    notification.text should be ("test 1")
  }

  it should "show list command without buttons when there are not enough notifications" in {
    val dialogue = for(
      _ <- send("/list");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминания:\n\n" +
        "Напоминание 1 о \"test 1\" в 2018-12-01T12:00\n" +
        "Напоминание 3 о \"rec test 1\"", Some(Nil)))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()

  }

  it should "not show list buttons when there are exactly three messages" in {
    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/list");
      _ <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"),l => !l.exists(_.name.contains("-"))))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "show list command with forward button when there are more messages and updates to show more" in {
    def hasOnlyOneButton(name: String)(b: List[Button]): Boolean =
      b.exists(_.name == name)


    val hasRightButton = hasOnlyOneButton("->")(_)
    val hasLeftButton = hasOnlyOneButton("<-")(_)

    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/in \"test 4\" завтра в 12:00");
      _ <- send("/list");
      m <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"), hasRightButton));
      _ <- sendCallback(m.buttons.find(_.name == "->").map(_.command).get);
      n <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("test 4"), hasLeftButton));
      _ <- sendCallback(n.buttons.find(_.name == "<-").map(_.command).get);
      _ <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("test 3"), hasRightButton))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "show list command with buttons that open message edit menu" in {

    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/in \"test 4\" завтра в 12:00");
      _ <- send("/in \"test 5\" завтра в 12:00");
      _ <- send("/list");
      m <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"), hasNavigationButtons));
      _ <- sendCallback(m.buttons.find(_.name == "1").map(_.command).get);
      n <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("Редактирование: test 1"), hasEditButtons));
      _ <- sendCallback(n.buttons.find(_.name == "Назад").map(_.command).get);
      _ <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("test 3"), hasNavigationButtons))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "show edit menu and allow notification date change" in {

    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/in \"test 4\" завтра в 12:00");
      _ <- send("/in \"test 5\" завтра в 12:00");
      _ <- send("/list");
      m <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"), hasNavigationButtons));
      _ <- sendCallback( m.buttons.find(_.name == "1").map(_.command).get);
      n <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("Редактирование: test 1"), hasEditButtons));
      _ <- sendCallback(n.buttons.find(_.name == "Перенести").map(_.command).get);
      _ <- shouldAnswerWith(sentMessage)(predicate(_.contains("Введите желаемое время для переноса напоминания:")))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "show edit menu and allow notification text change" in {

    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/in \"test 4\" завтра в 12:00");
      _ <- send("/in \"test 5\" завтра в 12:00");
      _ <- send("/list");
      m <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"), hasNavigationButtons));
      _ <- sendCallback( m.buttons.find(_.name == "1").map(_.command).get);
      n <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("Редактирование: test 1"), hasEditButtons));
      _ <- sendCallback(n.buttons.find(_.name == "Изменить").map(_.command).get);
      _ <- shouldAnswerWith(sentMessage)(predicate(_.contains("Введите желаемый текст для напоминания:")));
      _ <- send("TEST123");
      _ <- shouldAnswerWith(sentMessage)(predicate(_.contains("Текст напоминания изменен")));
      _ <- send("/list");
      _ <- shouldAnswerWith(sentMessage)(predicate(_.contains("TEST123")))
    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "show edit menu and allow notification delete" in {


    val dialogue = for(
      _ <- send("/in \"test 3\" сегодня в 22:00");
      _ <- send("/in \"test 4\" завтра в 12:00");
      _ <- send("/in \"test 5\" завтра в 12:00");
      _ <- send("/list");
      m <- shouldAnswerWith(sentMessage)(predicate(_.contains("test 3"), hasNavigationButtons));
      _ <- sendCallback(m.buttons.find(_.name == "1").map(_.command).get);
      n <- shouldAnswerWith(editOfMessage(m.id))(predicate(_.contains("Редактирование: test 1"), hasEditButtons));
      _ <- sendCallback(n.buttons.find(_.name == "Удалить").map(_.command).get);
      _ <- shouldAnswerWith(editOfMessage(m.id))(predicate(x => x != "test 1", hasNavigationButtons))

    ) yield ()

    dialogue
      .run(MockBotState(defaultUser :: Nil, notifications = existingNotifications.toList))
      .unsafeRunSync()
  }

  it should "work correctly in longer dialogue with creating, showing and deleting notifications" in {
    val dialogue = for(
      _ <- send("/in \"test 1\" 22.08 в 23:55");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено в 23:55"));
      _ <- send("/in \"test 2\" через 4 дня в это же время");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено 26.08 в 12:00"));
      _ <- send("/show");
      _ <- shouldAnswerWithTextThat(t => t.contains("test 1") && t.contains("test 2"));
      _ <- send("/in");
      _ <- send("test 3");
      _ <- send("послезавтра в 9:55");
      _ <- send("/delete 1");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание удалено"));
      _ <- send("/show");
      _ <- shouldAnswerWithTextThat(t => t.contains("test 2") && t.contains("test 3"))
    ) yield ()

    val (state, _) = dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()

    state.notifications.length should be (3)
  }

  it should "change notification date if requested" in {
    val dialogue = for(
      _ <- send("/in \"test 1\" 22.08 в 23:55");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено в 23:55"));
      _ <- send("/change 0");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Введите желаемое время для переноса напоминания:"));
      _ <- send("22.08 в 23:57");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено в 23:57"))
    ) yield ()

    dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()
  }

  it should "warn if notification id does not exist" in {
    val dialogue = for(
      _ <- send("/in \"test 1\" 22.08 в 23:55");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание поставлено и будет отправлено в 23:55"));
      _ <- send("/change 5");
      _ <- shouldAnswerWith(sentMessage)(hasExpected("Напоминание с id 5 не найдено"))
    ) yield ()

    dialogue.run(MockBotState(users = defaultUser :: Nil)).unsafeRunSync()
  }

  private[this] def sendCallback(command: String, user: User = defaultTgUser) =
    bot(ReceiveCallbackQuery(CallbackQuery("-1", defaultTgUser,
      Some(Message(-1, Some(user), 0, Chat(1, ChatType.Private))),
      chatInstance = "",
      data = Some(command))))

  private[this] def send(text: String, user: User = defaultTgUser) =
    bot(ReceiveMessage(Message(-1, Some(user), 0, Chat(1, ChatType.Private), text = Some(text))))

  private[this] def editOfMessage(id: Int)(list: List[MockMessage]): MockMessage = {
    list.exists(_.id == id) should be (true)

    list.find(_.id == id).get
  }

  private[this] def sentMessage(list: List[MockMessage]): MockMessage = {
    list.last
  }

  private[this] def shouldAnswerWith(f: List[MockMessage] => MockMessage)(assert: MockMessage => Assertion) = StateT.get[IO, MockBotState] map { state =>
    val msg = f(state.sentMessages)

    assert(msg)

    msg
  }

  private[this] def hasButtonsWithNames(names: String*)(buttons: List[Button]): Boolean =
    buttons.map(_.name) == names.toList

  private[this] def predicate(f: String => Boolean)(msg: MockMessage): Assertion =
    f(msg.text) should be (true)

  private[this] def predicate(f: String => Boolean, button: List[Button] => Boolean)(msg: MockMessage): Assertion =
    f(msg.text) && button(msg.buttons) should be (true)

  private[this] def hasExpected(text: String, button: Option[List[Button]] = None)(lastMessage: MockMessage): Assertion =
    lastMessage.text should be (text)

  private[this] def shouldAnswerWithTextThat(f: String => Boolean) =
    shouldAnswerWith(sentMessage)(predicate(f))

  private[this] val defaultUser = PersistedUser(1, Some(1), "test")
  private[this] val defaultTgUser = User(1, false, "1",username = Some("test"))
  private[this] val defaultUserId = defaultUser.id
  private[this] val mockedDateTime = MockDateTime(LocalDateTime.of(2018, 8, 22, 12, 0, 0))

  private[this] val hasNavigationButtons = hasButtonsWithNames("1","2","3", "->")(_)
  private[this] val hasEditButtons = hasButtonsWithNames("Назад", "Перенести", "Изменить","Удалить")(_)

  private[this] val helpText = "Бот c напоминаниями\n" +
    "/in - Напоминает о событии через заданный интервал времени\n" +
    "/show - Показывает активные напоминания\n" +
    "/delete <id> - Удаляет напоминания с указанным id\n" +
    "/change <id> - Изменяет дату и время на напоминании с указанным id\n" +
    "/list - Показывает новый список напоминаний"

  private[this] val existingNotifications =  Vector(
    Notification(1, defaultUserId, "test 1",true, OneDate(LocalDateTime.of(2018,12,1,12,0))),
    Notification(2, defaultUserId, "test 1 na", false, OneDate(LocalDateTime.of(2018,12,1,12,0))),
    Notification(3, defaultUserId, "rec test 1", true, Periodic(LocalDateTime.now(), 10, 10, Set(1), None, None)),
    Notification(4, defaultUserId, "rec test 1 - na", false, Periodic(LocalDateTime.now(), 10, 10, Set(1), None, None)),
    Notification(5, defaultUserId + 1, "asdasd", true, Periodic(LocalDateTime.now(), 10, 10, Set(1), None, None))

  )

  private[this] val random: Random[MockBotF] =
    (int: Int) => StateT.pure[IO, MockBotState, Int](new util.Random(1).nextInt(int))

  private[this] val parser = new CombinatorMomentInFutureParser(
    new DatesAst[MockBotF](random),
    new DatesAst[MockBotF](random))

  private[this] def createBot(systemDateTime: SystemDateTime): NotificationBot[MockBotF] =
    NotificationBot[MockBotF](
      new MockUsersRepository(defaultUser),
      new MockChatStateRepository,
      new MockSender,
      parser,
      new MockNotificationRepository,
      new NotificationChatServiceImpl[MockBotF](
        new MockNotificationRepository,
        new MockUsersRepository(defaultUser),
        new MockChatStateRepository,
        parser,
        systemDateTime,
        new TimeBeautifyServiceImpl(systemDateTime),
        new MockSender
      ),
      new PageViewImpl[MockBotF](
        new MockNotificationRepository,
        new MockSender
      ),
      systemDateTime)

  private[this] val bot = createBot(mockedDateTime)

}
