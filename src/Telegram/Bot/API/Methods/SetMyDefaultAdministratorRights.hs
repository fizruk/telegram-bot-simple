{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMyDefaultAdministratorRights where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'setMyDefaultAdministratorRights'

-- | Request parameters for 'setMyDefaultAdministratorRights'.
data SetMyDefaultAdministratorRightsRequest = SetMyDefaultAdministratorRightsRequest
  { setMyDefaultAdministratorRightsRequestRights      :: Maybe ChatAdministratorRights -- ^ A JSON-serialized object describing new default administrator rights. If not specified, the default administrator rights will be cleared.
  , setMyDefaultAdministratorRightsRequestForChannels :: Maybe Bool -- ^ Pass 'True' to change the default administrator rights of the bot in channels. Otherwise, the default administrator rights of the bot for groups and supergroups will be changed.
  }
  deriving Generic

instance ToJSON   SetMyDefaultAdministratorRightsRequest where toJSON = gtoJSON
instance FromJSON SetMyDefaultAdministratorRightsRequest where parseJSON = gparseJSON

type SetMyDefaultAdministratorRights = "setMyDefaultAdministratorRights"
  :> ReqBody '[JSON] SetMyDefaultAdministratorRightsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the default administrator rights requested by the bot when it's added as an administrator to groups or channels. These rights will be suggested to users, but they are are free to modify the list before adding the bot. Returns 'True' on success.
setMyDefaultAdministratorRights
  :: SetMyDefaultAdministratorRightsRequest -> ClientM (Response Bool)
setMyDefaultAdministratorRights = client (Proxy @SetMyDefaultAdministratorRights)
