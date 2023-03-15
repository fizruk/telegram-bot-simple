{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetMyShortDescription where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Data.Text (Text)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setMyShortDescription'

data SetMyShortDescriptionRequest = SetMyShortDescriptionRequest
  { setMyShortDescriptionDescription  :: Maybe Text -- ^ New short description for the bot; 0-120 characters. Pass an empty string to remove the dedicated short description for the given language.
  , setMyShortDescriptionLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code. If empty, the short description will be applied to all users for whose language there is no dedicated short description.
  }
  deriving Generic

instance ToJSON   SetMyShortDescriptionRequest where toJSON = gtoJSON
instance FromJSON SetMyShortDescriptionRequest where parseJSON = gparseJSON

type SetMyShortDescription = "setMyShortDescription"
  :> ReqBody '[JSON] SetMyShortDescriptionRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the bot's short description,
--   which is shown on the bot's profile page and
--   is sent together with the link when users share the bot.
--   Returns 'True' on success.
setMyShortDescription :: SetMyShortDescriptionRequest -> ClientM (Response Bool)
setMyShortDescription = client (Proxy @SetMyShortDescription)

makeDefault ''SetMyShortDescriptionRequest
