{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.VideoChatScheduled where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'VideoChatScheduled'

-- | This object represents a service message about a video chat scheduled in the chat.
data VideoChatScheduled = VideoChatScheduled
  { videoChatScheduledStartDate :: POSIXTime -- ^ Point in time (Unix timestamp) when the video chat is supposed to be started by a chat administrator.
  }
  deriving (Generic, Show)

instance ToJSON   VideoChatScheduled where toJSON = gtoJSON
instance FromJSON VideoChatScheduled where parseJSON = gparseJSON
