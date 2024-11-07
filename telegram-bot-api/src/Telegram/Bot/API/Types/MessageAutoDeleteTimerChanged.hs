{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.MessageAutoDeleteTimerChanged where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'MessageAutoDeleteTimerChanged'

-- | This object represents a service message about a change in auto-delete timer settings.
newtype MessageAutoDeleteTimerChanged = MessageAutoDeleteTimerChanged
  { messageAutoDeleteTimerChangedMessageAutoDeleteTime :: Seconds -- ^ New auto-delete time for messages in the chat; in seconds
  }
  deriving (Generic, Show)

instance ToJSON   MessageAutoDeleteTimerChanged where toJSON = gtoJSON
instance FromJSON MessageAutoDeleteTimerChanged where parseJSON = gparseJSON
