{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.MessageReactionCountUpdated where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.ReactionCount
import Telegram.Bot.API.Internal.Utils

-- ** 'MessageReactionCountUpdated'

-- | This object represents reaction changes on a message with anonymous reactions.
data MessageReactionCountUpdated = MessageReactionCountUpdated
  { messageReactionCountUpdatedChat :: Chat -- ^ The chat containing the message.
  , messageReactionCountUpdatedMessageId :: MessageId -- ^ Unique message identifier inside the chat.
  , messageReactionCountUpdatedDate :: POSIXTime -- ^ Date of the change in Unix time.
  , messageReactionCountUpdatedReactions :: [ReactionCount] -- ^ List of reactions that are present on the message.
  }
  deriving (Generic, Show)

instance ToJSON   MessageReactionCountUpdated where toJSON = gtoJSON
instance FromJSON MessageReactionCountUpdated where parseJSON = gparseJSON
