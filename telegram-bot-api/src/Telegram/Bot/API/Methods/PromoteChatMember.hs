{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.PromoteChatMember where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'promoteChatMember'

-- | Request parameters for 'promoteChatMember'.
data PromoteChatMemberRequest = PromoteChatMemberRequest
  { promoteChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , promoteChatMemberUserId :: UserId -- ^ Unique identifier of the target user.
  , promoteChatMemberIsAnonymous :: Maybe Bool -- ^ Pass 'True', if the administrator's presence in the chat is hidden.
  , promoteChatMemberCanManageChat :: Maybe Bool -- ^ Pass 'True', if the administrator can access the chat event log, chat statistics, message statistics in channels, see channel members, see anonymous administrators in supergroups and ignore slow mode. Implied by any other administrator privilege.
  , promoteChatMemberCanPostMessages :: Maybe Bool -- ^ Pass 'True', if the administrator can create channel posts, channels only.
  , promoteChatMemberCanEditMessages :: Maybe Bool -- ^ Pass 'True', if the administrator can edit messages of other users and can pin messages, channels only.
  , promoteChatMemberCanDeleteMessages :: Maybe Bool -- ^ Pass 'True', if the administrator can delete messages of other users.
  , promoteChatMemberCanManageVideoChats :: Maybe Bool -- ^ Pass 'True', if the administrator can manage video chats.
  , promoteChatMemberCanRestrictMembers :: Maybe Bool -- ^ Pass 'True', if the administrator can restrict, ban or unban chat members.
  , promoteChatMemberCanPromoteMembers :: Maybe Bool -- ^ Pass 'True', if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by him).
  , promoteChatMemberCanChangeInfo :: Maybe Bool -- ^ Pass 'True', if the administrator can change chat title, photo and other settings.
  , promoteChatMemberCanInviteUsers :: Maybe Bool -- ^ Pass 'True', if the administrator can invite new users to the chat.
  , promoteChatMemberCanPinMessages :: Maybe Bool -- ^ Pass 'True', if the administrator can pin messages, supergroups only.
  , promoteChatMemberCanPostStories :: Maybe Bool -- ^ Pass 'True' if the administrator can post stories in the channel; channels only.
  , promoteChatMemberCanEditStories :: Maybe Bool -- ^ Pass 'True' if the administrator can edit stories posted by other users; channels only.
  , promoteChatMemberCanDeleteStories :: Maybe Bool -- ^ Pass 'True' if the administrator can delete stories posted by other users; channels only.
  , promoteChatMemberCanManageTopics :: Maybe Bool -- ^ Pass 'True', if the user is allowed to create, rename, close, and reopen forum topics, supergroups only.
  }
  deriving Generic

instance ToJSON   PromoteChatMemberRequest where toJSON = gtoJSON
instance FromJSON PromoteChatMemberRequest where parseJSON = gparseJSON

type PromoteChatMember = "promoteChatMember"
  :> ReqBody '[JSON] PromoteChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to promote or demote
--   a user in a supergroup or a channel.
--   The bot must be an administrator in
--   the chat for this to work and must have
--   the appropriate administrator rights.
--   Pass False for all boolean parameters
--   to demote a user.
--   Returns True on success.
promoteChatMember ::PromoteChatMemberRequest ->  ClientM (Response Bool)
promoteChatMember = client (Proxy @PromoteChatMember)

makeDefault ''PromoteChatMemberRequest
