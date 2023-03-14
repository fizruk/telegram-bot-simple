{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetMyDescription where

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

-- ** 'GetMyDescription'

newtype GetMyDescriptionRequest = GetMyDescriptionRequest
  { getMyDescriptionLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code or an empty string.
  }
  deriving Generic

instance ToJSON   GetMyDescriptionRequest where toJSON = gtoJSON
instance FromJSON GetMyDescriptionRequest where parseJSON = gparseJSON

type GetMyDescription = "getMyDescription"
  :> ReqBody '[JSON] GetMyDescriptionRequest
  :> Post '[JSON] (Response BotDescription)

-- | Use this method to get the current bot description for the given user language.
--   Returns 'BotDescription' on success.
getMyDescription :: GetMyDescriptionRequest -> ClientM (Response BotDescription)
getMyDescription = client (Proxy @GetMyDescription)

makeDefault ''GetMyDescriptionRequest