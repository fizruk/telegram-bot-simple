{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForumTopicCreated where

import Data.Aeson (FromJSON (..), ToJSON (..))
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

instance ToJSON   ForumTopicCreated where toJSON = gtoJSON
instance FromJSON ForumTopicCreated where parseJSON = gparseJSON
