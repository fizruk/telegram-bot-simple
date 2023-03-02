{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.DeleteMyCommands where

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

-- ** 'deleteMyCommands'

-- | Request parameters for 'deleteMyCommands'.
data DeleteMyCommandsRequest = DeleteMyCommandsRequest
  { deleteMyCommandsScope :: Maybe BotCommandScope  -- ^ A JSON-serialized object, describing scope of users. Defaults to BotCommandScopeDefault.
  , deleteMyCommandsLanguageCode :: Maybe Text  -- ^ 	A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
  }
  deriving Generic

instance ToJSON   DeleteMyCommandsRequest where toJSON = gtoJSON
instance FromJSON DeleteMyCommandsRequest where parseJSON = gparseJSON

type DeleteMyCommands = "deleteMyCommands"
  :> ReqBody '[JSON] DeleteMyCommandsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete the list of
--   the bot's commands for the given scope
--   and user language. After deletion, higher
--   level commands will be shown to affected users.
--   Returns True on success.
deleteMyCommands :: DeleteMyCommandsRequest -> ClientM (Response Bool)
deleteMyCommands = client (Proxy @DeleteMyCommands)

makeDefault ''DeleteMyCommandsRequest
