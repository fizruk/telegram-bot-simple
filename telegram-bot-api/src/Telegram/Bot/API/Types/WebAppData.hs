{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.WebAppData where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'WebAppData'

data WebAppData = WebAppData
  { webAppDataData       :: Text -- ^ The data. Be aware that a bad client can send arbitrary data in this field.
  , webAppDataButtonText :: Text -- ^ Text of the @web_app@ keyboard button, from which the Web App was opened. Be aware that a bad client can send arbitrary data in this field.
  }
  deriving (Generic, Show)

instance ToJSON   WebAppData where toJSON = gtoJSON
instance FromJSON WebAppData where parseJSON = gparseJSON
