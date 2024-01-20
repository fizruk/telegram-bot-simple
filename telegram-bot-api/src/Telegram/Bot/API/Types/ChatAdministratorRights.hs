{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatAdministratorRights where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ChatAdministratorRights'

-- | Represents the rights of an administrator in a chat.
data ChatAdministratorRights = ChatAdministratorRights
  { chatAdministratorRightsIsAnonymous         :: Bool -- ^ 'True', if the user's presence in the chat is hidden.
  , chatAdministratorRightsCanManageChat       :: Bool -- ^ 'True', if the administrator can access the chat event log, chat statistics, message statistics in channels, see channel members, see anonymous administrators in supergroups and ignore slow mode. Implied by any other administrator privilege.
  , chatAdministratorRightsCanDeleteMessages   :: Bool -- ^ 'True', if the administrator can delete messages of other users.
  , chatAdministratorRightsCanManageVideoChats :: Bool -- ^ 'True', if the administrator can manage video chats.
  , chatAdministratorRightsCanRestrictMembers  :: Bool -- ^ 'True', if the administrator can restrict, ban or unban chat members.
  , chatAdministratorRightsCanPromoteMembers   :: Bool -- ^ 'True', if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by the user).
  , chatAdministratorRightsCanChangeInfo       :: Bool -- ^ 'True', if the user is allowed to change the chat title, photo and other settings.
  , chatAdministratorRightsCanInviteUsers      :: Bool -- ^ 'True', if the user is allowed to invite new users to the chat.
  , chatAdministratorRightsCanPostMessages     :: Maybe Bool -- ^ 'True', if the administrator can post in the channel; channels only.
  , chatAdministratorRightsCanEditMessages     :: Maybe Bool -- ^ 'True', if the administrator can edit messages of other users and can pin messages; channels only.
  , chatAdministratorRightsCanPinMessages      :: Maybe Bool -- ^ 'True', if the user is allowed to pin messages; groups and supergroups only
  , chatAdministratorRightsCanPostStories      :: Maybe Bool -- ^ 'True', if the administrator can post stories in the channel; channels only.
  , chatAdministratorRightsCanEditStories      :: Maybe Bool -- ^ 'True', if the administrator can edit stories posted by other users; channels only.
  , chatAdministratorRightsCanDeleteStories    :: Maybe Bool -- ^ 'True', if the administrator can delete stories posted by other users; channels only.
  , chatAdministratorRightsCanManageTopics     :: Maybe Bool -- ^ 'True', if the user is allowed to create, rename, close, and reopen forum topics; supergroups only.
  }
  deriving (Generic, Show)

instance ToJSON   ChatAdministratorRights where toJSON = gtoJSON
instance FromJSON ChatAdministratorRights where parseJSON = gparseJSON
