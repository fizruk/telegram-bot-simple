{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForumTopic where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'ForumTopic'

-- | This object represents a forum topic.
data ForumTopic = ForumTopic
  { forumTopicMessageThreadId   :: MessageThreadId -- ^ Unique identifier of the forum topic
  , forumTopicName              :: Text            -- ^ Name of the topic
  , forumTopicIconColor         :: Integer         -- ^ Color of the topic icon in RGB format.
  , forumTopicIconCustomEmojiId :: Maybe Text      -- ^ Unique identifier of the custom emoji shown as the topic icon.
  }
  deriving (Generic, Show)

instance ToJSON   ForumTopic where toJSON = gtoJSON
instance FromJSON ForumTopic where parseJSON = gparseJSON
