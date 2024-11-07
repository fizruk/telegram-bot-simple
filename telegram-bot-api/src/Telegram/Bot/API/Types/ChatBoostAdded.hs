{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatBoostAdded where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ChatBoostAdded'

-- | This object represents a boost added to a chat or changed.
newtype ChatBoostAdded = ChatBoostAdded
  { chatBoostAddedBoostCount :: Int -- ^ Number of boosts added by the user.
  }
  deriving (Generic, Show)

instance ToJSON   ChatBoostAdded where toJSON = gtoJSON
instance FromJSON ChatBoostAdded where parseJSON = gparseJSON
