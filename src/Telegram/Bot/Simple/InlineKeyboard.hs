module Telegram.Bot.Simple.InlineKeyboard where

import           Data.Text        (Text)
import qualified Data.Text        as Text

import           Telegram.Bot.API

urlButton :: Text -> Text -> InlineKeyboardButton
urlButton label url = (labeledInlineKeyboardButton label)
  { inlineKeyboardButtonUrl = Just url}

callbackButton :: Text -> Text -> InlineKeyboardButton
callbackButton label data_ = (labeledInlineKeyboardButton label)
  { inlineKeyboardButtonCallbackData = Just data_}

actionButton :: Show action => Text -> action -> InlineKeyboardButton
actionButton label = callbackButton label . Text.pack . show
