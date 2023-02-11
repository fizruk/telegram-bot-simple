{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetChatMember where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getChatMember'

type GetChatMember = "getChatMember"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response ChatMember)

-- | Use this method to get information about a member of a chat. 
--   Returns a ChatMember object on success.
getChatMember :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ 	Unique identifier of the target user
  -> ClientM (Response ChatMember)
getChatMember = client (Proxy @GetChatMember)
