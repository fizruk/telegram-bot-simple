{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.UnpinChatMessage where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'unpinChatMessage'

type UnpinChatMessage = "unpinChatMessage"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> QueryParam "message_id" MessageId
  :> Post '[JSON] (Response Bool)

-- | Use this method to remove a message from the
--   list of pinned messages in a chat. If the chat 
--   is not a private chat, the bot must be an administrator
--   in the chat for this to work and must have the 
--   'can_pin_messages' administrator right in a supergroup 
--   or 'can_edit_messages' administrator right in a 
--   channel. 
--   Returns True on success.
unpinChatMessage :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Maybe MessageId -- ^ Identifier of a message to unpin. If not specified, the most recent pinned message (by sending date) will be unpinned.
  -> ClientM (Response Bool)
unpinChatMessage = client (Proxy @UnpinChatMessage)
