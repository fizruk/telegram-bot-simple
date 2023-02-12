{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatInviteLink where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ChatInviteLink'

-- | Represents an invite link for a chat.
data ChatInviteLink = ChatInviteLink
  { chatInviteLinkInviteLink              :: Text            -- ^ The invite link. If the link was created by another chat administrator, then the second part of the link will be replaced with “…”.
  , chatInviteLinkCreator                 :: User            -- ^ Creator of the link.
  , chatInviteLinkCreatesJoinRequest      :: Bool            -- ^ 'True', if users joining the chat via the link need to be approved by chat administrators.
  , chatInviteLinkIsPrimary               :: Bool            -- ^ 'True', if the link is primary.
  , chatInviteLinkIsRevoked               :: Bool            -- ^ 'True', if the link is revoked.
  , chatInviteLinkName                    :: Maybe Text      -- ^ Invite link name.
  , chatInviteLinkExpireDate              :: Maybe POSIXTime -- ^ Point in time (Unix timestamp) when the link will expire or has been expired.
  , chatInviteLinkMemberLimit             :: Maybe Int     -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999.
  , chatInviteLinkPendingJoinRequestCount :: Maybe Int     -- ^ Number of pending join requests created using this link.
  }
  deriving (Generic, Show)

instance ToJSON   ChatInviteLink where toJSON = gtoJSON
instance FromJSON ChatInviteLink where parseJSON = gparseJSON
