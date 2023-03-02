{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetChatMemberCount where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getChatMemberCount'

type GetChatMemberCount = "getChatMemberCount"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Integer)

-- | Use this method to get the number of members in a chat.
--   Returns Int on success.
getChatMemberCount :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Integer)
getChatMemberCount = client (Proxy @GetChatMemberCount)
