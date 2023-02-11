{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ForumTopicEdited where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ForumTopicEdited'

-- | This object represents a service message about an edited forum topic.
data ForumTopicEdited = ForumTopicEdited
  { forumTopicEditedName              :: Maybe Text -- ^ New name of the topic, if it was edited.
  , forumTopicEditedIconCustomEmojiId :: Maybe Text -- ^ New identifier of the custom emoji shown as the topic icon, if it was edited; an empty string if the icon was removed.
  }
  deriving (Generic, Show)

instance ToJSON   ForumTopicEdited where toJSON = gtoJSON
instance FromJSON ForumTopicEdited where parseJSON = gparseJSON
