{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.PollOption where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'PollOption'

-- | This object contains information about one answer option in a poll.
data PollOption = PollOption
  { pollOptionText       :: Text -- ^ Option text, 1-100 characters.
  , pollOptionVoterCount :: Int  -- ^ Number of users that voted for this option.
  }
  deriving (Generic, Show)

instance ToJSON   PollOption where toJSON = gtoJSON
instance FromJSON PollOption where parseJSON = gparseJSON
