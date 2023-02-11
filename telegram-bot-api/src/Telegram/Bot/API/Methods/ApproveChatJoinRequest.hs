{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.ApproveChatJoinRequest where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'approveChatJoinRequest'

type ApproveChatJoinRequest = "approveChatJoinRequest"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response Bool)

-- | Use this method to approve a chat 
--   join request. The bot must be an 
--   administrator in the chat for this 
--   to work and must have the can_invite_users 
--   administrator right. 
--   Returns True on success.
approveChatJoinRequest :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ Unique identifier of the target user
  -> ClientM (Response Bool)
approveChatJoinRequest = client (Proxy @ApproveChatJoinRequest)
