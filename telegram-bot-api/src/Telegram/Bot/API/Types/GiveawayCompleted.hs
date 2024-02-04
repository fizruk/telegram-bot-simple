{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GiveawayCompleted where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Message

-- ** 'GiveawayCompleted'

-- | This object represents a service message about the completion of a giveaway without public winners.
data GiveawayCompleted = GiveawayCompleted
  { giveawayCompletedWinnerCount :: Int -- ^ Number of winners in the giveaway.
  , giveawayCompletedUnclaimedPrizeCount :: Maybe Int -- ^ Number of undistributed prizes.
  , giveawayCompletedGiveawayMessage :: Maybe Message -- ^ Message with the giveaway that was completed, if it wasn't deleted.
  }
  deriving (Generic, Show)

instance ToJSON   GiveawayCompleted where toJSON = gtoJSON
instance FromJSON GiveawayCompleted where parseJSON = gparseJSON
