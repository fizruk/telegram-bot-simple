{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatType where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ChatType'

-- | Type of the chat, can be either “private”, “group”, “supergroup” or “channel”.

data ChatType
  = ChatTypePrivate
  | ChatTypeGroup
  | ChatTypeSupergroup
  | ChatTypeChannel
  | ChatTypeSender
  deriving (Generic, Show)

instance ToJSON   ChatType where
  toJSON = gtoJSON
instance FromJSON ChatType where
  parseJSON = gparseJSON
