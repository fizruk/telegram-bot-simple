{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Location where

import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** Location

-- | This object represents a point on the map.
data Location = Location
  { locationLongitude            :: Float         -- ^ Longitude as defined by sender.
  , locationLatitude             :: Float         -- ^ Latitude as defined by sender.
  , locationHorizontalAccuracy   :: Maybe Float   -- ^ The radius of uncertainty for the location, measured in meters; 0-1500.
  , locationLivePeriod           :: Maybe Seconds -- ^ Time relative to the message sending date, during which the location can be updated; in seconds. For active live locations only.
  , locationHeading              :: Maybe Int     -- ^ The direction in which user is moving, in degrees; 1-360. For active live locations only.
  , locationProximityAlertRadius :: Maybe Int     -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. For sent live locations only.
  }
  deriving (Generic, Show)

deriveJSON' ''Location
