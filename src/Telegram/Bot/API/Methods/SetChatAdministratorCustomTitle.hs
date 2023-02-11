{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetChatAdministratorCustomTitle where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setChatAdministratorCustomTitle'

-- | Request parameters for 'setChatAdministratorCustomTitle'.
data SetChatAdministratorCustomTitleRequest = SetChatAdministratorCustomTitleRequest
  { setChatAdministratorCustomTitleChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , setChatAdministratorCustomTitleUserId :: UserId -- ^ Unique identifier of the target user
  , setChatAdministratorCustomTitleCustomTitle :: Text -- ^ New custom title for the administrator; 0-16 characters, emoji are not allowed
  }
  deriving Generic

instance ToJSON   SetChatAdministratorCustomTitleRequest where toJSON = gtoJSON
instance FromJSON SetChatAdministratorCustomTitleRequest where parseJSON = gparseJSON

type SetChatAdministratorCustomTitle = "setChatAdministratorCustomTitle"
  :> ReqBody '[JSON] SetChatAdministratorCustomTitleRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a custom title
--   for an administrator in a supergroup
--   promoted by the bot.
--   Returns True on success.
setChatAdministratorCustomTitle :: SetChatAdministratorCustomTitleRequest ->  ClientM (Response Bool)
setChatAdministratorCustomTitle = client (Proxy @SetChatAdministratorCustomTitle)
