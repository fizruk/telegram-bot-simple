{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatJoinRequest where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatInviteLink
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ChatJoinRequest'

-- | Represents a join request sent to a chat.
data ChatJoinRequest = ChatJoinRequest
  { chatJoinRequestChat       :: Chat                 -- ^ Chat to which the request was sent.
  , chatJoinRequestFrom       :: User                 -- ^ User that sent the join request.
  , chatJoinRequestUserChatId :: ChatId               -- ^ Identifier of a private chat with the user who sent the join request. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot can use this identifier for 24 hours to send messages until the join request is processed, assuming no other administrator contacted the user.
  , chatJoinRequestDate       :: POSIXTime            -- ^ Date the request was sent in Unix time.
  , chatJoinRequestBio        :: Maybe Text           -- ^ Bio of the user.
  , chatJoinRequestInviteLink :: Maybe ChatInviteLink -- ^ Chat invite link that was used by the user to send the join request.
  }
  deriving (Generic, Show)

instance ToJSON   ChatJoinRequest where toJSON = gtoJSON
instance FromJSON ChatJoinRequest where parseJSON = gparseJSON

