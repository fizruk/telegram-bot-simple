{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Poll where

import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Types.PollOption
import Telegram.Bot.API.Types.PollType
import Telegram.Bot.API.Internal.Utils

-- ** 'Poll'

data Poll = Poll
  { pollId                    :: PollId                -- ^ Unique poll identifier.
  , pollQuestion              :: Text                  -- ^ Poll question, 1-300 characters.
  , pollOptions               :: [PollOption]          -- ^ List of poll options.
  , pollTotalVoterCount       :: Int                 -- ^ Total number of users that voted in the poll.
  , pollIsClosed              :: Bool                  -- ^ 'True', if the poll is closed.
  , pollIsAnonymous           :: Bool                  -- ^ 'True', if the poll is anonymous.
  , pollType                  :: PollType              -- ^ Poll type, currently can be “regular” or “quiz”.
  , pollAllowsMultipleAnswers :: Bool                  -- ^ 'True', if the poll allows multiple answers.
  , pollCorrectOptionId       :: Maybe Int             -- ^ 0-based identifier of the correct answer option. Available only for polls in the quiz mode, which are closed, or was sent (not forwarded) by the bot or to the private chat with the bot.
  , pollExplanation           :: Maybe Text            -- ^ Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters.
  , pollExplanationEntities   :: Maybe [MessageEntity] -- ^ Special entities like usernames, URLs, bot commands, etc. that appear in the explanation.
  , pollOpenPeriod            :: Maybe Seconds         -- ^ Amount of time in seconds the poll will be active after creation.
  , pollCloseData             :: Maybe POSIXTime       -- ^ Point in time (Unix timestamp) when the poll will be automatically closed.
  }
  deriving (Generic, Show)

deriveJSON' ''Poll
