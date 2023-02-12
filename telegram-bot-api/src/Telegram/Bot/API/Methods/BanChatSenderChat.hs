{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.BanChatSenderChat where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'banChatSenderChat'

type BanChatSenderChat = "banChatSenderChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sender_chat_id" ChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to ban a channel chat
--   in a supergroup or a channel. Until the
--   chat is unbanned, the owner of the banned
--   chat won't be able to send messages on
--   behalf of any of their channels. The bot
--   must be an administrator in the supergroup
--   or channel for this to work and must have
--   the appropriate administrator rights.
--   Returns True on success.
banChatSenderChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ChatId -- ^ Unique identifier of the target sender chat
  -> ClientM (Response  Bool)
banChatSenderChat = client (Proxy @BanChatSenderChat)
