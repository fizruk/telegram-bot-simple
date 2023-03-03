{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.LeaveChat where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'leaveChat'

type LeaveChat = "leaveChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method for your bot to leave a group, supergroup or channel.
--   Returns True on success.
leaveChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
leaveChat = client (Proxy @LeaveChat)
