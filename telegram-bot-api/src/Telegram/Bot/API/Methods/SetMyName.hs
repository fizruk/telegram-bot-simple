{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMyName where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setMyName'

data SetMyNameRequest = SetMyNameRequest
  { setMyNameName  :: Maybe Text -- ^ New bot name; 0-64 characters. Pass an empty string to remove the dedicated name for the given language.
  , setMyNameLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code. If empty, the name will be shown to all users for whose language there is no dedicated name.
  }
  deriving Generic

instance ToJSON   SetMyNameRequest where toJSON = gtoJSON
instance FromJSON SetMyNameRequest where parseJSON = gparseJSON

type SetMyName = "setMyName"
  :> ReqBody '[JSON] SetMyNameRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the bot's name.
--   Returns 'True' on success.
setMyName :: SetMyNameRequest -> ClientM (Response Bool)
setMyName = client (Proxy @SetMyName)

makeDefault ''SetMyNameRequest
