{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.SwitchInlineQueryChosenChat where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'SwitchInlineQueryChosenChat'

-- | This object represents an inline button that switches the current user to inline mode in a chosen chat, with an optional default inline query.
data SwitchInlineQueryChosenChat = SwitchInlineQueryChosenChat
  { switchInlineQueryChosenChatQuery :: Maybe Text -- ^ The default inline query to be inserted in the input field. If left empty, only the bot's username will be inserted.
  , switchInlineQueryChosenChatAllowUserChats :: Maybe Bool -- ^ 'True', if private chats with users can be chosen.
  , switchInlineQueryChosenChatAllowBotChats :: Maybe Bool -- ^ 'True', if private chats with bots can be chosen.
  , switchInlineQueryChosenChatAllowGroupChats :: Maybe Bool -- ^ 'True', if group and supergroup chats can be chosen.
  , switchInlineQueryChosenChatAllowChannelChats :: Maybe Bool -- ^ 'True', if channel chats can be chosen.
  }
  deriving (Show, Generic)

instance ToJSON   SwitchInlineQueryChosenChat where toJSON = gtoJSON
instance FromJSON SwitchInlineQueryChosenChat where parseJSON = gparseJSON
