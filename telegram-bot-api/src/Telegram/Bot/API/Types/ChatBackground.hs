{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatBackground where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.BackgroundType

-- ** 'ChatBackground'

-- | This object represents a chat background.
newtype ChatBackground = ChatBackground
  { chatBackgroundType :: BackgroundType -- ^ Type of the background.
  }
  deriving (Generic, Show)

instance ToJSON   ChatBackground where toJSON = gtoJSON
instance FromJSON ChatBackground where parseJSON = gparseJSON
