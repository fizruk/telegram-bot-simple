{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForumTopicClosed where

import Data.Aeson (FromJSON (..), ToJSON (..), Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ForumTopicClosed'

-- | This object represents a service message about a forum topic closed in the chat. Currently holds no information.
newtype ForumTopicClosed = ForumTopicClosed Object
  deriving (Generic, Show)

instance ToJSON   ForumTopicClosed where toJSON = gtoJSON
instance FromJSON ForumTopicClosed where parseJSON = gparseJSON
