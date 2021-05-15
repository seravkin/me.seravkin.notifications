package me.seravkin.notifications.infrastructure.telegram

import cats.effect._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.cats.TelegramBot
import com.bot4s.telegram.models._
import events._
import org.asynchttpclient.Dsl.asyncHttpClient
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend

/**
 * Adapter from Bot[IO] to telegrambot4s bot API
 *
 * @param token tg bot token
 * @param botFactory function to create bot from request handler
 */
class TelegramBotAdapter(token: String, botFactory: RequestHandler[IO] => IO[Bot[IO]])(implicit contextShift: ContextShift[IO])
  extends TelegramBot[IO](token, AsyncHttpClientCatsBackend.usingClient[IO](asyncHttpClient())) {

  def this(token: String, botFactory: RequestHandler[IO] => Bot[IO])(implicit dummyImplicit: DummyImplicit, contextShift: ContextShift[IO]) = {
    this(token, r => IO(botFactory(r)))
  }

  private[this] val constructed = botFactory(request).unsafeRunSync()

  private[this] def run(botEvent: BotEvent): IO[Unit] =
    constructed(botEvent)

  override def receiveMessage(message: Message): IO[Unit] =
    run(ReceiveMessage(message))

  override def receiveEditedMessage(editedMessage: Message): IO[Unit] =
    run(ReceiveEditedMessage(editedMessage))

  override def receiveChannelPost(message: Message): IO[Unit] =
    run(ReceiveChannelPost(message))

  override def receiveEditedChannelPost(message: Message): IO[Unit] =
    run(ReceiveEditedChannelPost(message))

  override def receiveInlineQuery(inlineQuery: InlineQuery): IO[Unit] =
    run(ReceiveInlineQuery(inlineQuery))

  override def receiveChosenInlineResult(chosenInlineResult: ChosenInlineResult): IO[Unit] =
    run(ReceiveChosenInlineResult(chosenInlineResult))

  override def receiveCallbackQuery(callbackQuery: CallbackQuery): IO[Unit] =
    run(ReceiveCallbackQuery(callbackQuery))

  override def receiveShippingQuery(shippingQuery: ShippingQuery): IO[Unit] =
    run(ReceiveShippingQuery(shippingQuery))

  override def receivePreCheckoutQuery(preCheckoutQuery: PreCheckoutQuery): IO[Unit] =
    run(ReceivePreCheckoutQuery(preCheckoutQuery))
}