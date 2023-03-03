{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetChatAdministrators where


import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getChatAdministrators'

type GetChatAdministrators = "getChatAdministrators"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response [ChatMember])

-- | Use this method to get a list of administrators
--   in a chat. On success, returns an Array of
--   ChatMember objects that contains information
--   about all chat administrators except other bots.
--   If the chat is a group or a supergroup and no
--   administrators were appointed, only the creator
--   will be returned.
getChatAdministrators :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response [ChatMember])
getChatAdministrators = client (Proxy @GetChatAdministrators)
