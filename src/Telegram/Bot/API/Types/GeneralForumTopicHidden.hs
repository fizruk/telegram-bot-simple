{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.GeneralForumTopicHidden where

import Data.Aeson (Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'GeneralForumTopicHidden'

-- | This object represents a service message about General forum topic hidden in the chat. Currently holds no information.
newtype GeneralForumTopicHidden = GeneralForumTopicHidden Object
  deriving (Generic, Show)

deriveJSON' ''GeneralForumTopicHidden
