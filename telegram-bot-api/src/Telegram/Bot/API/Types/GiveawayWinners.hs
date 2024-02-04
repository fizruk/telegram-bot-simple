{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GiveawayWinners where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import {-# SOURCE #-} Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User

-- ** 'GiveawayWinners'

-- | This object represents a message about the completion of a giveaway with public winners.
data GiveawayWinners = GiveawayWinners
  { giveawayWinnersChat :: Chat -- ^ The chat that created the giveaway.
  , giveawayWinnersGiveawayMessageId :: MessageId -- ^ Identifier of the messsage with the giveaway in the chat.
  , giveawayWinnersWinnersSelectionDate :: POSIXTime -- ^ Point in time (Unix timestamp) when winners of the giveaway were selected.
  , giveawayWinnersWinnerCount :: Int -- ^ Total number of winners in the giveaway.
  , giveawayWinnersWinners :: [User] -- ^ List of up to 100 winners of the giveaway.
  , giveawayWinnersAdditionalChatCount :: Maybe Int -- ^ The number of other chats the user had to join in order to be eligible for the giveaway.
  , giveawayWinnersPremiumSubscriptionMonthCount :: Maybe Int -- ^ The number of months the Telegram Premium subscription won from the giveaway will be active for.
  , giveawayWinnersUnclaimedPrizeCount :: Maybe Int -- ^ Number of undistributed prizes.
  , giveawayWinnersOnlyNewMembers :: Maybe Bool -- ^ 'True', if only users who had joined the chats after the giveaway started were eligible to win.
  , giveawayWinnersWasRefunded :: Maybe Bool -- ^ 'True', if the giveaway was canceled because the payment for it was refunded.
  , giveawayWinnersPrizeDescription :: Maybe Text -- ^ Description of additional giveaway prize.
  }
  deriving (Generic, Show)

instance ToJSON   GiveawayWinners where toJSON = gtoJSON
instance FromJSON GiveawayWinners where parseJSON = gparseJSON
