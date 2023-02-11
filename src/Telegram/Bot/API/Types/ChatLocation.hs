{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatLocation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Location
import Telegram.Bot.API.Internal.Utils

-- ** 'ChatLocation'

-- | Represents a location to which a chat is connected.
data ChatLocation = ChatLocation
  { chatLocationLocation :: Location -- ^ The location to which the supergroup is connected. Can't be a live location..
  , chatLocationAddress :: Text      -- ^ Location address; 1-64 characters, as defined by the chat owner.
  }
  deriving (Generic, Show)

instance ToJSON   ChatLocation where toJSON = gtoJSON
instance FromJSON ChatLocation where parseJSON = gparseJSON
