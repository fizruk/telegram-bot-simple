{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForceReply where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ForceReply'

-- | Upon receiving a message with this object,
-- Telegram clients will display a reply interface to the user
-- (act as if the user has selected the bot‘s message and tapped ’Reply').
-- This can be extremely useful if you want to create user-friendly
-- step-by-step interfaces without having to sacrifice privacy mode.
data ForceReply = ForceReply
  { forceReplyForceReply            :: Bool       -- ^ Shows reply interface to the user, as if they manually selected the bot‘s message and tapped ’Reply'
  , forceReplyInputFieldPlaceholder :: Maybe Text -- ^ The placeholder to be shown in the input field when the reply is active; 1-64 characters.
  , forceReplySelective             :: Maybe Bool -- ^ Use this parameter if you want to force reply from specific users only. Targets: 1) users that are @mentioned in the text of the Message object; 2) if the bot's message is a reply (has reply_to_message_id), sender of the original message.
  }
  deriving (Generic, Show)

instance ToJSON   ForceReply where toJSON = gtoJSON
instance FromJSON ForceReply where parseJSON = gparseJSON
