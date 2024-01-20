{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Story where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'Story'

-- | This object represents a message about a forwarded story in the chat. Currently holds no information.

data Story = Story
  deriving (Generic, Show)

instance ToJSON Story where toJSON = gtoJSON
instance FromJSON Story where parseJSON = gparseJSON
