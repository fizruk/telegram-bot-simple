{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetChat where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getChat'

type GetChat = "getChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response ChatFullInfo)

-- | Use this method to get up to date information
--   about the chat (current name of the user for
--   one-on-one conversations, current username of
--   a user, group or channel, etc.).
--   Returns a Chat object on success.
getChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response ChatFullInfo)
getChat = client (Proxy @GetChat)
