{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatMenuButton where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'setChatMenuButton'

-- | Request parameters for 'setChatMenuButton'.
data SetChatMenuButtonRequest = SetChatMenuButtonRequest
  { setChatMenuButtonRequestChatId     :: Maybe ChatId     -- ^ Unique identifier for the target private chat. If not specified, default bot's menu button will be changed.
  , setChatMenuButtonRequestMenuButton :: Maybe MenuButton -- ^ A JSON-serialized object for the new bot's menu button. Defaults to @MenuButtonDefault@.
  }
  deriving Generic

instance ToJSON   SetChatMenuButtonRequest where toJSON = gtoJSON
instance FromJSON SetChatMenuButtonRequest where parseJSON = gparseJSON

type SetChatMenuButton = "setChatMenuButton"
  :> ReqBody '[JSON] SetChatMenuButtonRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the bot's menu button in a private chat,
--  or the default menu button. Returns True on success.
setChatMenuButton :: SetChatMenuButtonRequest -> ClientM (Response Bool)
setChatMenuButton = client (Proxy @SetChatMenuButton)

makeDefault ''SetChatMenuButtonRequest
