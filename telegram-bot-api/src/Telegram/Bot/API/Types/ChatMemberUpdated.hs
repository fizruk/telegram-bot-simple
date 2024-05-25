{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatMemberUpdated where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatInviteLink
import Telegram.Bot.API.Types.ChatMember
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ChatMemberUpdated'

-- | This object represents changes in the status of a chat member.
data ChatMemberUpdated = ChatMemberUpdated
  { chatMemberUpdatedChat          :: Chat                 -- ^ Chat the user belongs to.
  , chatMemberUpdatedFrom          :: User                 -- ^ Performer of the action, which resulted in the change.
  , chatMemberUpdatedDate          :: POSIXTime            -- ^ Date the change was done in Unix time.
  , chatMemberUpdatedOldChatMember :: ChatMember           -- ^ Previous information about the chat member.
  , chatMemberUpdatedNewChatMember :: ChatMember           -- ^ New information about the chat member.
  , chatMemberUpdatedInviteLink    :: Maybe ChatInviteLink -- ^ Chat invite link, which was used by the user to join the chat; for joining by invite link events only.
  , chatMemberUpdatedViaJoinRequest :: Maybe Bool          -- ^ 'True', if the user joined the chat after sending a direct join request without using an invite link and being approved by an administrator.
  , chatMemberUpdatedViaChatFolderInviteLink :: Maybe Bool -- ^ 'True', if the user joined the chat via a chat folder invite link.
  }
  deriving (Generic, Show)

instance ToJSON   ChatMemberUpdated where toJSON = gtoJSON
instance FromJSON ChatMemberUpdated where parseJSON = gparseJSON


