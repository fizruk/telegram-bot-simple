{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetMyDefaultAdministratorRights where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'getMyDefaultAdministratorRights'

-- | Request parameters for 'getMyDefaultAdministratorRights'.
newtype GetMyDefaultAdministratorRightsRequest = GetMyDefaultAdministratorRightsRequest
  { getMyDefaultAdministratorRightsRequestForChannels :: Maybe Bool -- ^ Pass 'True' to get default administrator rights of the bot in channels. Otherwise, default administrator rights of the bot for groups and supergroups will be returned.
  }
  deriving Generic

instance ToJSON   GetMyDefaultAdministratorRightsRequest where toJSON = gtoJSON
instance FromJSON GetMyDefaultAdministratorRightsRequest where parseJSON = gparseJSON

type GetMyDefaultAdministratorRights = "getMyDefaultAdministratorRights"
  :> ReqBody '[JSON] GetMyDefaultAdministratorRightsRequest
  :> Post '[JSON] (Response ChatAdministratorRights)

-- | Use this method to get the current default administrator rights of the bot.
-- Returns 'ChatAdministratorRights' on success.
getMyDefaultAdministratorRights
  :: GetMyDefaultAdministratorRightsRequest -> ClientM (Response ChatAdministratorRights)
getMyDefaultAdministratorRights = client (Proxy @GetMyDefaultAdministratorRights)

makeDefault ''GetMyDefaultAdministratorRightsRequest
