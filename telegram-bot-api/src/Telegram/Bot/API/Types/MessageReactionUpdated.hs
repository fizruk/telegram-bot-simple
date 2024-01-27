{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.MessageReactionUpdated where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.ReactionType
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'MessageReactionUpdated'

-- | This object represents a change of a reaction on a message performed by a user.
data MessageReactionUpdated = MessageReactionUpdated
  { messageReactionUpdatedChat :: Chat -- ^ The chat containing the message.
  , messageReactionUpdatedMessageId :: MessageId -- ^ Unique message identifier inside the chat.
  , messageReactionUpdatedUser :: Maybe User -- ^ The user that changed the reaction, if the user isn't anonymous.
  , messageReactionUpdatedActorChat :: Maybe Chat -- ^ The chat on behalf of which the reaction was changed, if the user is anonymous.
  , messageReactionUpdatedDate :: POSIXTime -- ^ Date of the change in Unix time.
  , messageReactionUpdatedOldReaction :: [ReactionType] -- ^ Previous list of reaction types that were set by the user.
  , messageReactionUpdatedNewReaction :: [ReactionType] -- ^ New list of reaction types that have been set by the user.
  }
  deriving (Generic, Show)

instance ToJSON   MessageReactionUpdated where toJSON = gtoJSON
instance FromJSON MessageReactionUpdated where parseJSON = gparseJSON
