{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BusinessMessagesDeleted where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'BusinessMessagesDeleted'

-- | This object is received when messages are deleted from a connected business account.
data BusinessMessagesDeleted = BusinessMessagesDeleted
  { businessMessagesDeletedBusinessConnectionId :: BusinessConnectionId -- ^ Unique identifier of the business connection.
  , businessMessagesDeletedChat :: Chat -- ^ Information about a chat in the business account. The bot may not have access to the chat or the corresponding user.
  , businessMessagesDeletedMessageIds :: [MessageId] -- ^ The list of identifiers of deleted messages in the chat of the business account.
  }
  deriving (Generic, Show)

instance ToJSON   BusinessMessagesDeleted where toJSON = gtoJSON
instance FromJSON BusinessMessagesDeleted where parseJSON = gparseJSON
