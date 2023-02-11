{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.InlineKeyboardButton where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.CallbackGame
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'InlineKeyboardButton'

-- | This object represents one button of an inline keyboard. You must use exactly one of the optional fields.
data InlineKeyboardButton = InlineKeyboardButton
  { inlineKeyboardButtonText              :: Text -- ^ Label text on the button
  , inlineKeyboardButtonUrl               :: Maybe Text -- ^ HTTP url to be opened when button is pressed
  , inlineKeyboardButtonCallbackData      :: Maybe Text -- ^ Data to be sent in a callback query to the bot when button is pressed, 1-64 bytes
  , inlineKeyboardButtonWebApp            :: Maybe WebAppInfo -- ^ Description of the Web App that will be launched when the user presses the button. The Web App will be able to send an arbitrary message on behalf of the user using the method @answerWebAppQuery@. Available only in private chats between a user and the bot.
  , inlineKeyboardButtonSwitchInlineQuery :: Maybe Text -- ^ If set, pressing the button will prompt the user to select one of their chats, open that chat and insert the bot‘s username and the specified inline query in the input field. Can be empty, in which case just the bot’s username will be inserted.
  , inlineKeyboardButtonSwitchInlineQueryCurrentChat :: Maybe Text -- ^ If set, pressing the button will insert the bot‘s username and the specified inline query in the current chat's input field. Can be empty, in which case only the bot’s username will be inserted.

  , inlineKeyboardButtonCallbackGame      :: Maybe CallbackGame -- ^ Description of the game that will be launched when the user presses the button.
  , inlineKeyboardButtonPay               :: Maybe Bool -- ^ Specify True, to send a Pay button.
  }
  deriving (Generic, Show)

labeledInlineKeyboardButton :: Text -> InlineKeyboardButton
labeledInlineKeyboardButton label = InlineKeyboardButton label Nothing Nothing Nothing Nothing Nothing Nothing Nothing

deriveJSON' ''InlineKeyboardButton
