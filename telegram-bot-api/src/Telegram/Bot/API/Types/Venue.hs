{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Venue where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Location
import Telegram.Bot.API.Internal.Utils

-- ** 'Venue'

-- | This object represents a venue.
data Venue = Venue
  { venueLocation        :: Location   -- ^ Venue location.
  , venueTitle           :: Text       -- ^ Name of the venue.
  , venueAddress         :: Text       -- ^ Address of the venue.
  , venueFoursquareId    :: Maybe Text -- ^ Foursquare identifier of the venue.
  , venueFoursquareType  :: Maybe Text -- ^ Foursquare type of the venue. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  , venueGooglePlaceId   :: Maybe Text -- ^ Google Places identifier of the venue.
  , venueGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See supported types.)
  }
  deriving (Generic, Show)

instance ToJSON   Venue where toJSON = gtoJSON
instance FromJSON Venue where parseJSON = gparseJSON
