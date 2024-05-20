{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessOpeningHoursInterval where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'BusinessOpeningHoursInterval'

-- | Contains information about the start page settings of a Telegram Business account.
data BusinessOpeningHoursInterval = BusinessOpeningHoursInterval
  { businessOpeningHoursIntervalOpeningMinute :: Int -- ^ The minute's sequence number in a week, starting on Monday, marking the start of the time interval during which the business is open; @0 - 7 * 24 * 60@.
  , businessOpeningHoursIntervalClosingMinute :: Int -- ^ The minute's sequence number in a week, starting on Monday, marking the end of the time interval during which the business is open; @0 - 8 * 24 * 60@.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessOpeningHoursInterval where toJSON = gtoJSON
instance FromJSON BusinessOpeningHoursInterval where parseJSON = gparseJSON
