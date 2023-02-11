{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GeneralForumTopicHidden where

import Data.Aeson (FromJSON (..), ToJSON (..), Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'GeneralForumTopicHidden'

-- | This object represents a service message about General forum topic hidden in the chat. Currently holds no information.
newtype GeneralForumTopicHidden = GeneralForumTopicHidden Object
  deriving (Generic, Show)

instance ToJSON   GeneralForumTopicHidden where toJSON = gtoJSON
instance FromJSON GeneralForumTopicHidden where parseJSON = gparseJSON
