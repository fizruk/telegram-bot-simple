{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.PollOption where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.MessageEntity

-- ** 'PollOption'

-- | This object contains information about one answer option in a poll.
data PollOption = PollOption
  { pollOptionText       :: Text -- ^ Option text, 1-100 characters.
  , pollOptionEntities  :: Maybe [MessageEntity] -- ^ Special entities that appear in the option @text@. Currently, only custom emoji entities are allowed in poll option texts.
  , pollOptionVoterCount :: Int  -- ^ Number of users that voted for this option.
  }
  deriving (Generic, Show)

instance ToJSON   PollOption where toJSON = gtoJSON
instance FromJSON PollOption where parseJSON = gparseJSON
