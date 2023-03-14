{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMyDescription where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setMyDescription'

data SetMyDescriptionRequest = SetMyDescriptionRequest
  { setMyDescriptionDescription  :: Maybe Text -- ^ New bot description; 0-512 characters. Pass an empty string to remove the dedicated description for the given language.
  , setMyDescriptionLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code. If empty, the description will be applied to all users for whose language there is no dedicated description.
  }
  deriving Generic

instance ToJSON   SetMyDescriptionRequest where toJSON = gtoJSON
instance FromJSON SetMyDescriptionRequest where parseJSON = gparseJSON

type SetMyDescription = "setMyDescription"
  :> ReqBody '[JSON] SetMyDescriptionRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the bot's description,
--   which is shown in the chat with the bot if the chat is empty.
--   Returns 'True' on success.
setMyDescriptionRequest :: SetMyDescriptionRequest -> ClientM (Response Bool)
setMyDescriptionRequest = client (Proxy @SetMyDescription)

makeDefault ''SetMyDescriptionRequest