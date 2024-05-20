{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Telegram.Bot.API.Types.Story where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Chat


-- ** 'Story'

-- | Unique identifier for the story in the chat
newtype StoryId = StoryId Int
  deriving (Show, ToJSON, FromJSON)

-- | This object represents a message about a story.
data Story = Story
  { storyChat :: Chat -- ^ Chat that posted the story.
  , storyId :: StoryId -- ^ Unique identifier for the story in the chat.
  }
  deriving (Generic, Show)

instance ToJSON Story where toJSON = gtoJSON
instance FromJSON Story where parseJSON = gparseJSON
