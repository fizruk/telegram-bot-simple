{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.InlineKeyboardMarkup where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.InlineKeyboardButton
import Telegram.Bot.API.Internal.Utils

-- ** 'InlineKeyboardMarkup'

-- | This object represents an inline keyboard that appears
-- right next to the message it belongs to.
newtype InlineKeyboardMarkup = InlineKeyboardMarkup
  { inlineKeyboardMarkupInlineKeyboard :: [[InlineKeyboardButton]] -- ^ Array of button rows, each represented by an Array of InlineKeyboardButton objects
  }
  deriving (Generic, Show)

-- ^
-- **Note**: This will only work in Telegram versions released after 9 April, 2016. Older clients will display unsupported message.

instance ToJSON   InlineKeyboardMarkup where toJSON = gtoJSON
instance FromJSON InlineKeyboardMarkup where parseJSON = gparseJSON
