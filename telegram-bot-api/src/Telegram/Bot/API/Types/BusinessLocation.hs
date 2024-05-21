{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessLocation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Location

-- ** 'BusinessLocation'

-- | Contains information about the location of a Telegram Business account.
data BusinessLocation = BusinessLocation
  { businessLocationAddress :: Text -- ^ Address of the business.
  , businessLocationLocation :: Maybe Location -- ^ Location of the business.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessLocation where toJSON = gtoJSON
instance FromJSON BusinessLocation where parseJSON = gparseJSON
