{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Giveaway where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Chat

-- ** 'Giveaway'

-- | This object represents a message about a scheduled giveaway.
data Giveaway = Giveaway
  { giveawayChats :: [Chat] -- ^ The list of chats which the user must join to participate in the giveaway
  , giveawayWinnersSelectionDate :: POSIXTime -- ^ Point in time (Unix timestamp) when winners of the giveaway will be selected.
  , giveawayWinnerCount :: Int -- ^ The number of users which are supposed to be selected as winners of the giveaway.
  , giveawayOnlyNewMembers :: Maybe Bool -- ^ 'True', if only users who join the chats after the giveaway started should be eligible to win.
  , giveawayHasPublicWinners :: Maybe Bool -- ^ 'True', if the list of giveaway winners will be visible to everyone.
  , giveawayPrizeDescription :: Maybe Text -- ^ Description of additional giveaway prize.
  , giveawayCountryCodes :: Maybe [Text] -- ^ A list of two-letter ISO 3166-1 alpha-2 country codes indicating the countries from which eligible users for the giveaway must come. If empty, then all users can participate in the giveaway. Users with a phone number that was bought on Fragment can always participate in giveaways.
  , giveawayPremiumSubscriptionMonthCount :: Maybe Int -- ^ The number of months the Telegram Premium subscription won from the giveaway will be active for.
  }
  deriving (Generic, Show)

instance ToJSON   Giveaway where toJSON = gtoJSON
instance FromJSON Giveaway where parseJSON = gparseJSON
