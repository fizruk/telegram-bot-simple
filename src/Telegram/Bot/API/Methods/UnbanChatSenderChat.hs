{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.UnbanChatSenderChat where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'unbanChatSenderChat'

type UnbanChatSenderChat = "unbanChatSenderChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sender_chat_id" ChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to unban a previously
--   banned channel chat in a supergroup
--   or channel. The bot must be an administrator
--   for this to work and must have the appropriate
--   administrator rights.
--   Returns True on success.
unbanChatSenderChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ChatId -- ^ Unique identifier of the target sender chat
  -> ClientM (Response  Bool)
unbanChatSenderChat = client (Proxy @UnbanChatSenderChat)
