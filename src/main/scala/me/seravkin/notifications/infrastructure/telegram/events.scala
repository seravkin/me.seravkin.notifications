package me.seravkin.notifications.infrastructure.telegram

import com.bot4s.telegram.models.{CallbackQuery, ChosenInlineResult, InlineQuery, Message, PreCheckoutQuery, ShippingQuery}

object events {
    /**
     * Telegram API event
     */
    sealed trait BotEvent

    /**
     * Message received
     * @param message telegrambot4s message
     */
    final case class ReceiveMessage(message: Message) extends BotEvent

    /**
     * Message edit receive
     * @param message telegrambot4s message
     */
    final case class ReceiveEditedMessage(message: Message) extends BotEvent

    /**
     * Channel post received
     * @param message telegrambot4s channel post
     */
    final case class ReceiveChannelPost(message: Message) extends BotEvent

    /**
     * Channel post edit received
     * @param message telegrambot4s channel post edit
     */
    final case class ReceiveEditedChannelPost(message: Message) extends BotEvent

    /**
     * Inline query received
     * @param inlineQuery telegrambot4s inline query
     */
    final case class ReceiveInlineQuery(inlineQuery: InlineQuery) extends BotEvent

    /**
     * Inline result received
     * @param chosenInlineResult telegrambot4s chosen inline query
     */
    final case class ReceiveChosenInlineResult(chosenInlineResult: ChosenInlineResult) extends BotEvent

    /**
     * Callback query received
     * @param callbackQuery telegrambot4s callback query
     */
    final case class ReceiveCallbackQuery(callbackQuery: CallbackQuery) extends BotEvent

    /**
     * Shipping query received
     * @param shippingQuery telegrambot4s shipping query
     */
    final case class ReceiveShippingQuery(shippingQuery: ShippingQuery) extends BotEvent

    /**
     * Pre checkout query received
     * @param preCheckoutQuery telegrambot4s pre checkout query
     */
    final case class ReceivePreCheckoutQuery(preCheckoutQuery: PreCheckoutQuery) extends BotEvent
}
