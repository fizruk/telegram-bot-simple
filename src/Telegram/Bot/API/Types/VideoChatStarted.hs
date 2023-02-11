{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.VideoChatStarted where

import Data.Aeson
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'VideoChatStarted'

-- | This object represents a service message about a video chat started in the chat. Currently holds no information.
data VideoChatStarted = VideoChatStarted
  deriving (Generic, Show)

instance ToJSON VideoChatStarted where
  toJSON = gtoJSON

instance FromJSON VideoChatStarted where
  parseJSON (Object _) = pure VideoChatStarted
  parseJSON _ = fail "Unable to parse VideoChatStarted: expected an empty object"
