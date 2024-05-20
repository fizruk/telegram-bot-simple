{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Telegram.Bot.API.Types.Birthdate where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Calendar
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils


-- ** 'Birthdate'

-- | Describes the birthdate of a user.
data Birthdate = Birthdate
  { birthdateDay :: DayOfMonth -- ^ Day of the user's birth; 1-31.
  , birthdateMonth :: MonthOfYear -- ^ Month of the user's birth; 1-12.
  , birthdate :: Maybe Year -- ^ Year of the user's birth.
  }
  deriving (Generic, Show)

instance ToJSON Birthdate where toJSON = gtoJSON
instance FromJSON Birthdate where parseJSON = gparseJSON
