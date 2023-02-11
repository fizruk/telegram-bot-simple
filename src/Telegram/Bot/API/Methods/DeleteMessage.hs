{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteMessage where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'deleteMessage'

-- | Notice that deleting by POST method was bugged, so we use GET
type DeleteMessage = "deleteMessage"
  :> RequiredQueryParam "chat_id" ChatId
  :> RequiredQueryParam "message_id" MessageId
  :> Get '[JSON] (Response Bool)

-- | Use this method to delete message in chat.
-- On success, the sent Bool is returned.
deleteMessage :: ChatId -> MessageId -> ClientM (Response Bool)
deleteMessage = client (Proxy @DeleteMessage)
