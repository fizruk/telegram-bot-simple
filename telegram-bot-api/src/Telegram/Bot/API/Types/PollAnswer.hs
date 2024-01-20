{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.PollAnswer where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'PollAnswer'

-- | This object represents an answer of a user in a non-anonymous poll.
data PollAnswer = PollAnswer
  { pollAnswerPollId    :: PollId -- ^ Unique poll identifier.
  , pollAnswerVoterChat :: Maybe Chat -- ^ The chat that changed the answer to the poll, if the voter is anonymous.
  , pollAnswerUser      :: Maybe User   -- ^ The user that changed the answer to the poll, if the voter isn't anonymous. For backward compatibility, the field user in such objects will contain the user @136817688@ (\@Channel_Bot).
  , pollAnswerOptionIds :: [Int]  -- ^ 0-based identifiers of answer options, chosen by the user. May be empty if the user retracted their vote.
  }
  deriving (Generic, Show)

instance ToJSON   PollAnswer where toJSON = gtoJSON
instance FromJSON PollAnswer where parseJSON = gparseJSON
