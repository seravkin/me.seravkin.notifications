package me.seravkin.notifications.test

import java.time.{Duration, LocalDateTime}

import cats._
import me.seravkin.notifications.domain.Notifications.Notification
import me.seravkin.notifications.domain.PersistedUser
import me.seravkin.notifications.domain.interpreter.Dates._
import me.seravkin.notifications.domain.services.{NotificationTasksService, NotificationTasksServiceImpl}
import me.seravkin.notifications.test.mocks._
import org.scalatest.{FlatSpec, Matchers}

class NotificationTasksServiceSpec extends FlatSpec with Matchers {

  "NotificationTaskService" should "send messages for one date notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, OneDate(date.plusMinutes(-5))),
      Notification(2, 1, "test2", true, OneDate(date.plusMinutes(-2))),
      Notification(3, 1, "test3", true, OneDate(date.plusMinutes(5))),
      Notification(4, 1, "test4", false, OneDate(date.plusMinutes(-100)))))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val messages = state.sentMessages.map(_.text)
    messages should be (List("test1", "test2"))
  }

  it should "have correct buttons for one date notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, OneDate(date.plusMinutes(-5))),
      Notification(2, 1, "test2", true, OneDate(date.plusMinutes(-2))),
      Notification(3, 1, "test3", true, OneDate(date.plusMinutes(5))),
      Notification(4, 1, "test4", false, OneDate(date.plusMinutes(-100)))))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val buttons = state.sentMessages.map(_.buttons.map(_.name)).head
    buttons should be (List("Перенести"))
  }


  it should "send messages for confirmation notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Confirmation(date.plusMinutes(-3), Duration.ofMinutes(5))),
      Notification(2, 1, "test2", true, Confirmation(date.plusMinutes(-2), Duration.ofMinutes(10))),
      Notification(3, 1, "test3", true, Confirmation(date.plusMinutes(5), Duration.ofMinutes(10))),
      Notification(4, 1, "test4", false, Confirmation(date.plusMinutes(-100), Duration.ofMinutes(5)))))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val messages = state.sentMessages.map(_.text)
    messages should be (List("test1", "test2"))
  }

  it should "compile notifications for confirmation notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Confirmation(date.plusMinutes(-3), Duration.ofMinutes(5))),
      Notification(2, 1, "test2", true, Confirmation(date.plusMinutes(-2), Duration.ofMinutes(10))),
      Notification(3, 1, "test3", true, Confirmation(date.plusMinutes(5), Duration.ofMinutes(10))),
      Notification(4, 1, "test4", false, Confirmation(date.plusMinutes(-100), Duration.ofMinutes(5)))))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val compiled = state.compiledDatesIdToDate

    compiled(1) should be (date.plusMinutes(5 - 3))
    compiled(2) should be (date.plusMinutes(10 - 2))
  }

  it should "have correct buttons for confirmation notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Confirmation(date.plusMinutes(-3), Duration.ofMinutes(5))),
      Notification(2, 1, "test2", true, Confirmation(date.plusMinutes(-2), Duration.ofMinutes(10))),
      Notification(3, 1, "test3", true, Confirmation(date.plusMinutes(5), Duration.ofMinutes(10))),
      Notification(4, 1, "test4", false, Confirmation(date.plusMinutes(-100), Duration.ofMinutes(5)))))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val buttons = state.sentMessages.map(_.buttons.map(_.name)).head
    buttons should be (List("Перенести", "Отменить"))
  }

  it should "send messages for recurrent notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Periodic(9, 59, (0 until 7).toSet, None, None)(date.minusDays(1)).get)
    ))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)

    val messages = state.sentMessages.map(_.text)

    messages should be (List("test1"))

    val compiled = state.compiledDatesIdToDate

    compiled(1) should be (LocalDateTime.of(2018, 10, 10, 9, 59).plusDays(1))

    val buttons = state.sentMessages.map(_.buttons.map(_.name)).head

    buttons should be (List("Перенести", "Отменить"))
  }

  it should "compile notifications for recurrent notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Periodic(9, 59, (0 until 7).toSet, None, None)(date.minusDays(1)).get)
    ))

    val service = notificationTaskService(date)
    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)


    val compiled = state.compiledDatesIdToDate
    compiled(1) should be (LocalDateTime.of(2018, 10, 10, 9, 59).plusDays(1))
  }

  it should "have correct buttons for recurrent notifications" in {
    val date = LocalDateTime.of(2018, 10, 10, 10, 10)

    val initialState = MockBotState(List(PersistedUser(1, Some(1),"")), List(
      Notification(1, 1, "test1", true, Periodic(9, 59, (0 until 7).toSet, None, None)(date.minusDays(1)).get)
    ))

    val service = notificationTaskService(date)

    val (state, _) = service.sendNotificationsIfNeeded().run(initialState)


    val buttons = state.sentMessages.map(_.buttons.map(_.name)).head
    buttons should be (List("Перенести", "Отменить"))
  }


  private[this] def notificationTaskService(now: LocalDateTime): NotificationTasksService[MockBotF] =
    new NotificationTasksServiceImpl[MockBotF](
      MockDateTime(now),
      new MockNotificationRepository[Id],
      new MockSender[Id]
    )

}
