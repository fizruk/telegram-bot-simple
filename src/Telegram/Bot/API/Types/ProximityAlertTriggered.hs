{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ProximityAlertTriggered where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ProximityAlertTriggered'

-- | This object represents the content of a service message, sent whenever a user in the chat triggers a proximity alert set by another user.
data ProximityAlertTriggered = ProximityAlertTriggered
  { proximityAlertTriggeredTraveler :: User  -- ^ User that triggered the alert.
  , proximityAlertTriggeredWatcher  :: User  -- ^ User that set the alert.
  , proximityAlertTriggeredDistance :: Int -- ^ The distance between the users.
  }
  deriving (Generic, Show)

instance ToJSON   ProximityAlertTriggered where toJSON = gtoJSON
instance FromJSON ProximityAlertTriggered where parseJSON = gparseJSON
