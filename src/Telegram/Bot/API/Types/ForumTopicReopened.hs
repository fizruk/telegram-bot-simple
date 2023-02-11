{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForumTopicReopened where

import Data.Aeson (FromJSON (..), ToJSON (..), Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ForumTopicReopened'

-- | This object represents a service message about a forum topic reopened in the chat. Currently holds no information.
newtype ForumTopicReopened = ForumTopicReopened Object
  deriving (Generic, Show)

instance ToJSON   ForumTopicReopened where toJSON = gtoJSON
instance FromJSON ForumTopicReopened where parseJSON = gparseJSON
