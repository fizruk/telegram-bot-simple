{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.UnpinAllChatMessages where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'unpinAllChatMessages'

type UnpinAllChatMessages = "unpinAllChatMessages"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to clear the list of pinned
--   messages in a chat. If the chat is not a private
--   chat, the bot must be an administrator in the
--   chat for this to work and must have the 'can_pin_messages'
--   administrator right in a supergroup or 'can_edit_messages'
--   administrator right in a channel.
--   Returns True on success.
unpinAllChatMessages :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
unpinAllChatMessages = client (Proxy @UnpinAllChatMessages)
