{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.VideoChatEnded where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'VideoChatEnded'

-- | This object represents a service message about a video chat ended in the chat.
data VideoChatEnded = VideoChatEnded
  { videoChatEndedDuration :: Seconds -- ^ Video chat duration in seconds.
  }
  deriving (Generic, Show)

instance ToJSON   VideoChatEnded where toJSON = gtoJSON
instance FromJSON VideoChatEnded where parseJSON = gparseJSON


-- ** 'VideoChatParticipantsInvited'

data VideoChatParticipantsInvited = VideoChatParticipantsInvited
  { videoChatParticipantsInvitedUsers :: Maybe [User] -- ^ New members that were invited to the video chat.
  }
  deriving (Generic, Show)

instance ToJSON   VideoChatParticipantsInvited where toJSON = gtoJSON
instance FromJSON VideoChatParticipantsInvited where parseJSON = gparseJSON
