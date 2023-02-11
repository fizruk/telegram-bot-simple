{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetChatMenuButton where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getChatMenuButton'

-- | Request parameters for 'getChatMenuButton'.
data GetChatMenuButtonRequest = GetChatMenuButtonRequest
  { getChatMenuButtonRequestChatId     :: Maybe ChatId     -- ^ Unique identifier for the target private chat. If not specified, default bot's menu button will be returned.
  }
  deriving Generic

instance ToJSON   GetChatMenuButtonRequest where toJSON = gtoJSON
instance FromJSON GetChatMenuButtonRequest where parseJSON = gparseJSON

type GetChatMenuButton = "getChatMenuButton"
  :> ReqBody '[JSON] GetChatMenuButtonRequest
  :> Post '[JSON] (Response MenuButton)

-- | Use this method to get the current value
--  of the bot's menu button in a private chat, or the default menu button.
-- Returns @MenuButton@ on success.
getChatMenuButton :: GetChatMenuButtonRequest -> ClientM (Response MenuButton)
getChatMenuButton = client (Proxy @GetChatMenuButton)
