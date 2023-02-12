{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetFile where

import Data.Proxy
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getFile'

type GetFile
  = "getFile"
  :> RequiredQueryParam "file_id" FileId
  :> Get '[JSON] (Response File)

getFile :: FileId -> ClientM (Response File)
getFile = client (Proxy @GetFile)
