{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GeneralForumTopicUnhidden where

import Data.Aeson (FromJSON (..), ToJSON (..), Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'GeneralForumTopicUnhidden'

-- | This object represents a service message about General forum topic unhidden in the chat. Currently holds no information.
newtype GeneralForumTopicUnhidden = GeneralForumTopicUnhidden Object
  deriving (Generic, Show)

instance ToJSON   GeneralForumTopicUnhidden where toJSON = gtoJSON
instance FromJSON GeneralForumTopicUnhidden where parseJSON = gparseJSON
