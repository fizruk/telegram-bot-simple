{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.EditChatInviteLink where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'editChatInviteLink'

-- | Request parameters for 'editChatInviteLink'.
data EditChatInviteLinkRequest = EditChatInviteLinkRequest
  { editChatInviteLinkChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editChatInviteLinkInviteLink :: Text -- ^	The invite link to edit
  , editChatInviteLinkName :: Maybe Text -- ^ Invite link name; 0-32 characters
  , editChatInviteLinkExpireDate :: Maybe Integer -- ^ Point in time (Unix timestamp) when the link will expire
  , editChatInviteLinkMemberLimit :: Maybe Int -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  , editChatInviteLinkCreatesJoinRequest :: Maybe Bool -- ^ True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
  }
  deriving Generic

instance ToJSON   EditChatInviteLinkRequest where toJSON = gtoJSON
instance FromJSON EditChatInviteLinkRequest where parseJSON = gparseJSON

type EditChatInviteLink = "editChatInviteLink"
  :> ReqBody '[JSON] EditChatInviteLinkRequest
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to edit a non-primary
--   invite link created by the bot. The 
--   bot must be an administrator in the 
--   chat for this to work and must have 
--   the appropriate administrator rights.
--   Returns the edited invite link as a ChatInviteLink object.
editChatInviteLink :: EditChatInviteLinkRequest ->  ClientM (Response ChatInviteLink)
editChatInviteLink = client (Proxy @EditChatInviteLink)
