{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMyCommands where

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

-- ** 'setMyCommands'

-- | Request parameters for 'setMyCommands'.
data SetMyCommandsRequest = SetMyCommandsRequest
  { setMyCommandsCommands :: [BotCommand] -- ^ A JSON-serialized list of bot commands to be set as the list of the bot's commands. At most 100 commands can be specified.
  , setMyCommandsScope :: Maybe BotCommandScope -- ^ A JSON-serialized object, describing scope of users for which the commands are relevant. Defaults to BotCommandScopeDefault.
  , setMyCommandsLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
  }
  deriving Generic

instance ToJSON   SetMyCommandsRequest where toJSON = gtoJSON
instance FromJSON SetMyCommandsRequest where parseJSON = gparseJSON

type SetMyCommands = "setMyCommands"
  :> ReqBody '[JSON] SetMyCommandsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the list of
--   the bot's commands. See <https:\/\/core.telegram.org\/bots#commands>
--   for more details about bot commands.
--   Returns True on success.
setMyCommands :: SetMyCommandsRequest ->  ClientM (Response Bool)
setMyCommands = client (Proxy @SetMyCommands)

makeDefault ''SetMyCommandsRequest
