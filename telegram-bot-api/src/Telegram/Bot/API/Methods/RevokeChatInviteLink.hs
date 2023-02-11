{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.RevokeChatInviteLink where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'revokeChatInviteLink'

type RevokeChatInviteLink = "revokeChatInviteLink"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "invite_link" Text
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to revoke an invite
--   link created by the bot. If the primary 
--   link is revoked, a new link is automatically 
--   generated. The bot must be an administrator 
--   in the chat for this to work and must have 
--   the appropriate administrator rights. 
--   Returns the revoked invite link as ChatInviteLink object.
revokeChatInviteLink :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ The invite link to revoke
  -> ClientM (Response  ChatInviteLink)
revokeChatInviteLink = client (Proxy @RevokeChatInviteLink)
