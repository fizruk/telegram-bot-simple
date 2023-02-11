{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.WriteAccessAllowed where

import Data.Aeson (Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'WriteAccessAllowed'

-- | This object represents a service message about a user allowing a bot added to the attachment menu to write messages. Currently holds no information.
newtype WriteAccessAllowed = WriteAccessAllowed Object
  deriving (Generic, Show)

deriveJSON' ''WriteAccessAllowed
