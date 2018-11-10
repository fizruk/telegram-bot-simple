{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Chat where

import Data.Coerce (coerce)
import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- * Available methods

-- ** 'kickChatMember'
type KickChatMember = "kickChatMember"
  :> RequiredQueryParam "chat_id" ChatId
  :> RequiredQueryParam "user_id" UserId
  :> Get '[JSON] (Response Bool)

-- | Use this method to kick user from chat.
-- On success, the sent Bool is returned.
kickChatMember :: ChatId -> UserId -> ClientM (Response Bool)
kickChatMember = client (Proxy @KickChatMember)
