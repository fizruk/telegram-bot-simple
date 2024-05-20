{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessConnection where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'BusinessConnection'

-- | Describes the connection of the bot with a business account.
data BusinessConnection = BusinessConnection
  { businessConnectionId :: BusinessConnectionId -- ^ Unique identifier of the business connection.
  , businessConnectionUser :: User -- ^ Business account user that created the business connection.
  , businessConnectionUserChatId :: ChatId -- ^ Identifier of a private chat with the user who created the business connection. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier.
  , businessConnectionDate :: POSIXTime -- ^ Date the connection was established in Unix time.
  , businessConnectionCanReply :: Bool -- ^ 'True', if the bot can act on behalf of the business account in chats that were active in the last 24 hours.
  , businessConnectionIsEnabled :: Bool -- ^ 'True', if the connection is active.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessConnection where toJSON = gtoJSON
instance FromJSON BusinessConnection where parseJSON = gparseJSON
