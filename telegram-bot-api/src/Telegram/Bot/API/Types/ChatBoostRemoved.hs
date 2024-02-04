{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatBoostRemoved where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatBoostSource
import Telegram.Bot.API.Types.Common

-- ** 'ChatBoostRemoved'

-- | This object represents a boost removed from a chat.
data ChatBoostRemoved = ChatBoostRemoved
  { chatBoostRemovedChat :: Chat -- ^ Chat which was boosted.
  , chatBoostRemovedBoostId :: BoostId -- ^ Unique identifier of the boost.
  , chatBoostRemovedRemoveDate :: POSIXTime -- ^ Point in time (Unix timestamp) when the boost was removed.
  , chatBoostRemovedSource :: ChatBoostSource -- ^ Source of the removed boost.
  }
  deriving (Generic, Show)

instance ToJSON   ChatBoostRemoved where toJSON = gtoJSON
instance FromJSON ChatBoostRemoved where parseJSON = gparseJSON
