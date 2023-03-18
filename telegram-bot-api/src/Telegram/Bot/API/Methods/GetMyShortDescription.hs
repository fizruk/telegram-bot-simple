{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetMyShortDescription where

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

-- ** 'GetMyShortDescription'

newtype GetMyShortDescriptionRequest = GetMyShortDescriptionRequest
  { getMyShortDescriptionLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code or an empty string.
  }
  deriving Generic

instance ToJSON   GetMyShortDescriptionRequest where toJSON = gtoJSON
instance FromJSON GetMyShortDescriptionRequest where parseJSON = gparseJSON

type GetMyShortDescription = "getMyShortDescription"
  :> ReqBody '[JSON] GetMyShortDescriptionRequest
  :> Post '[JSON] (Response BotShortDescription)

-- | Use this method to get the current bot short description for the given user language.
--   Returns 'BotShortDescription' on success.
getMyShortDescription :: GetMyShortDescriptionRequest -> ClientM (Response BotShortDescription)
getMyShortDescription = client (Proxy @GetMyShortDescription)

makeDefault ''GetMyShortDescriptionRequest
