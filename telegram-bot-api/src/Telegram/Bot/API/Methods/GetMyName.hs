{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetMyName where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'GetMyName'

newtype GetMyNameRequest = GetMyNameRequest
  { getMyNameLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code or an empty string.
  }
  deriving Generic

instance ToJSON   GetMyNameRequest where toJSON = gtoJSON
instance FromJSON GetMyNameRequest where parseJSON = gparseJSON

type GetMyName = "getMyName"
  :> ReqBody '[JSON] GetMyNameRequest
  :> Post '[JSON] (Response BotName)

-- | Use this method to get the current bot name for the given user language.
--   Returns 'BotName' on success.
getMyName :: GetMyNameRequest -> ClientM (Response BotName)
getMyName = client (Proxy @GetMyName)

makeDefault ''GetMyNameRequest
