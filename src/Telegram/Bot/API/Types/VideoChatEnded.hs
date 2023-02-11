{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.VideoChatEnded where

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

-- ** 'VideoChatParticipantsInvited'
data VideoChatParticipantsInvited = VideoChatParticipantsInvited
  { videoChatParticipantsInvitedUsers :: Maybe [User] -- ^ New members that were invited to the video chat.
  }
  deriving (Generic, Show)

foldMap deriveJSON'
  [ ''VideoChatEnded
  , ''VideoChatParticipantsInvited
  ]
