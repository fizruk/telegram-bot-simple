{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Chat where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.ChatType
import Telegram.Bot.API.Types.Common

-- ** 'Chat'

-- | This object represents a chat.
data Chat = Chat
  { chatId :: ChatId -- ^ Unique identifier for this chat. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , chatType :: ChatType -- ^ Type of the chat, can be either “private”, “group”, “supergroup” or “channel”
  , chatTitle :: Maybe Text -- ^ Title, for supergroups, channels and group chats.
  , chatUsername :: Maybe Text -- ^ Username, for private chats, supergroups and channels if available.
  , chatFirstName :: Maybe Text -- ^ First name of the other party in a private chat.
  , chatLastName :: Maybe Text -- ^ Last name of the other party in a private chat.
  , chatIsForum :: Maybe Bool -- ^ 'True', if the supergroup chat is a forum (has topics enabled).
  }
  deriving (Generic, Show)

instance ToJSON   Chat where toJSON = gtoJSON
instance FromJSON Chat where parseJSON = gparseJSON
