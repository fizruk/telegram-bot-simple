{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeclineChatJoinRequest where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'declineChatJoinRequest'

type DeclineChatJoinRequest = "declineChatJoinRequest"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response Bool)

-- | Use this method to decline a chat 
--   join request. The bot must be an 
--   administrator in the chat for this 
--   to work and must have the can_invite_users 
--   administrator right. 
--   Returns True on success.
declineChatJoinRequest :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ Unique identifier of the target user
  -> ClientM (Response Bool)
declineChatJoinRequest = client (Proxy @DeclineChatJoinRequest)
