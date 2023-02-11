{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.SomeReplyMarkup where

import Data.Aeson
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.ForceReply
import Telegram.Bot.API.Types.InlineKeyboardMarkup
import Telegram.Bot.API.Types.ReplyKeyboardMarkup
import Telegram.Bot.API.Types.ReplyKeyboardRemove

-- | Additional interface options.
-- A JSON-serialized object for an inline keyboard, custom reply keyboard,
-- instructions to remove reply keyboard or to force a reply from the user.
data SomeReplyMarkup
  = SomeInlineKeyboardMarkup InlineKeyboardMarkup
  | SomeReplyKeyboardMarkup  ReplyKeyboardMarkup
  | SomeReplyKeyboardRemove  ReplyKeyboardRemove
  | SomeForceReply           ForceReply
  deriving (Generic)

instance ToJSON   SomeReplyMarkup where toJSON = genericSomeToJSON
instance FromJSON SomeReplyMarkup where parseJSON = genericSomeParseJSON
