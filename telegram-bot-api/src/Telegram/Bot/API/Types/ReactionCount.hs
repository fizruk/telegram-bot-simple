{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ReactionCount where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.ReactionType
import Telegram.Bot.API.Internal.Utils

-- ** 'ReactionCount'

-- | Represents a reaction added to a message along with the number of times it was added.
data ReactionCount = ReactionCount
      { reactionCountType :: ReactionType -- ^ Type of the reaction.
      , reactionCountTotalCount :: Int -- ^ Number of times the reaction was added.
      }
  deriving (Generic, Show)

instance ToJSON   ReactionCount where toJSON = gtoJSON
instance FromJSON ReactionCount where parseJSON = gparseJSON
