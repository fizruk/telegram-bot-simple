{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessOpeningHours where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.BusinessOpeningHoursInterval

-- ** 'BusinessOpeningHours'

-- | 
data BusinessOpeningHours = BusinessOpeningHours
  { businessOpeningHoursTimeZoneName :: Text -- ^ Unique name of the time zone for which the opening hours are defined.
  , businessOpeningHoursOpeningHours :: [BusinessOpeningHoursInterval] -- ^ List of time intervals describing business opening hours.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessOpeningHours where toJSON = gtoJSON
instance FromJSON BusinessOpeningHours where parseJSON = gparseJSON
