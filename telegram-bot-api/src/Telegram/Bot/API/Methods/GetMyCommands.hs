{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetMyCommands where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'getMyCommands'

-- | Request parameters for 'getMyCommands'.
data GetMyCommandsRequest = GetMyCommandsRequest
  { getMyCommandsScope :: Maybe BotCommandScope  -- ^ A JSON-serialized object, describing scope of users. Defaults to BotCommandScopeDefault.
  , getMyCommandsLanguageCode :: Maybe Text   -- ^ 	A two-letter ISO 639-1 language code or an empty string
  }
  deriving Generic

instance ToJSON   GetMyCommandsRequest where toJSON = gtoJSON
instance FromJSON GetMyCommandsRequest where parseJSON = gparseJSON

type GetMyCommands = "getMyCommands"
  :> ReqBody '[JSON] GetMyCommandsRequest
  :> Post '[JSON] (Response [BotCommand])

-- | Use this method to get the current list
--   of the bot's commands for the given scope
--   and user language. Returns Array of BotCommand
--   on success. If commands aren't set, an empty list
--   is returned.
getMyCommands :: GetMyCommandsRequest -> ClientM (Response [BotCommand])
getMyCommands = client (Proxy @GetMyCommands)

makeDefault ''GetMyCommandsRequest
