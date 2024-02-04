{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatBoost where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.ChatBoostSource
import Telegram.Bot.API.Types.Common

-- ** 'ChatBoost'

-- | This object contains information about a chat boost.
data ChatBoost = ChatBoost
  { chatBoostBoostId :: BoostId -- ^ Unique identifier of the boost.
  , chatBoostAddTime :: POSIXTime -- ^ Point in time (Unix timestamp) when the chat was boosted.
  , chatBoostExpirateionDate :: POSIXTime -- ^ Point in time (Unix timestamp) when the boost will automatically expire, unless the booster's Telegram Premium subscription is prolonged.
  , chatBoostSource :: ChatBoostSource -- ^ Source of the added boost.
  }
  deriving (Generic, Show)

instance ToJSON   ChatBoost where toJSON = gtoJSON
instance FromJSON ChatBoost where parseJSON = gparseJSON
