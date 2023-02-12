{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.CreateChatInviteLink where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'createChatInviteLink'

-- | Request parameters for 'createChatInviteLink'.
data CreateChatInviteLinkRequest = CreateChatInviteLinkRequest
  { createChatInviteLinkChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , createChatInviteLinkName :: Maybe Text -- ^ Invite link name; 0-32 characters
  , createChatInviteLinkExpireDate :: Maybe Integer -- ^ Point in time (Unix timestamp) when the link will expire
  , createChatInviteLinkMemberLimit :: Maybe Int -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  , createChatInviteLinkCreatesJoinRequest :: Maybe Bool -- ^ True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
  }
  deriving Generic

instance ToJSON   CreateChatInviteLinkRequest where toJSON = gtoJSON
instance FromJSON CreateChatInviteLinkRequest where parseJSON = gparseJSON

type CreateChatInviteLink = "createChatInviteLink"
  :> ReqBody '[JSON] CreateChatInviteLinkRequest
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to create an additional
--   invite link for a chat. The bot must be 
--   an administrator in the chat for this to 
--   work and must have the appropriate administrator 
--   rights. The link can be revoked using the 
--   method revokeChatInviteLink. 
--   Returns the new invite link as ChatInviteLink object.
createChatInviteLink :: CreateChatInviteLinkRequest ->  ClientM (Response ChatInviteLink)
createChatInviteLink = client (Proxy @CreateChatInviteLink)
