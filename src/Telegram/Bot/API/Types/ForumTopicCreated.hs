{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.ForumTopicCreated where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ForumTopicCreated'

-- | This object represents a service message about a new forum topic created in the chat.
data ForumTopicCreated = ForumTopicCreated
  { forumTopicCreatedName              :: Text       -- ^ Name of the topic.
  , forumTopicCreatedIconColor         :: Integer    -- ^ Color of the topic icon in RGB format.
  , forumTopicCreatedIconCustomEmojiId :: Maybe Text -- ^ Unique identifier of the custom emoji shown as the topic icon.
  }
  deriving (Generic, Show)

deriveJSON' ''ForumTopicCreated
