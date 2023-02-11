{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.ExportChatInviteLink where

import Data.Proxy
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'exportChatInviteLink'

type ExportChatInviteLink = "exportChatInviteLink"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Text)

-- | Use this method to generate a new
--   primary invite link for a chat; any
--   previously generated primary link is
--   revoked. The bot must be an administrator
--   in the chat for this to work and must have
--   the appropriate administrator rights.
--   Returns the new invite link as String on success.
exportChatInviteLink :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response  Text)
exportChatInviteLink = client (Proxy @ExportChatInviteLink)
