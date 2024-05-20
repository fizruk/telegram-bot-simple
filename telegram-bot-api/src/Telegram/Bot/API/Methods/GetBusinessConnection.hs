{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetBusinessConnection where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getBusinessConnection'

type GetBusinessConnection = "getBusinessConnection"
  :> RequiredQueryParam "business_connection_id" BusinessConnectionId
  :> Post '[JSON] (Response BusinessConnection)

-- | Use this method to get information about the connection of the bot with a business account. Returns a 'BusinessConnection' object on success.
getBusinessConnection :: BusinessConnectionId -> ClientM (Response BusinessConnection)
getBusinessConnection = client (Proxy @GetBusinessConnection)
