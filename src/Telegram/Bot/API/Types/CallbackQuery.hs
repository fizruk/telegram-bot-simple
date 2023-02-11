{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.CallbackQuery where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.Message
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'CallbackQuery'

-- | This object represents an incoming callback query from a callback button
-- in an inline keyboard. If the button that originated the query was attached
-- to a message sent by the bot, the field message will be present.
-- If the button was attached to a message sent via the bot (in inline mode),
-- the field @inline_message_id@ will be present.
-- Exactly one of the fields data or game_short_name will be present.
data CallbackQuery = CallbackQuery
  { callbackQueryId              :: CallbackQueryId -- ^ Unique identifier for this query
  , callbackQueryFrom            :: User -- ^ Sender
  , callbackQueryMessage         :: Maybe Message -- ^ Message with the callback button that originated the query. Note that message content and message date will not be available if the message is too old
  , callbackQueryInlineMessageId :: Maybe MessageId -- ^ Identifier of the message sent via the bot in inline mode, that originated the query.
  , callbackQueryChatInstance    :: Text -- ^ Global identifier, uniquely corresponding to the chat to which the message with the callback button was sent. Useful for high scores in games.
  , callbackQueryData            :: Maybe Text -- ^ Data associated with the callback button. Be aware that a bad client can send arbitrary data in this field.
  , callbackQueryGameShortName   :: Maybe Text -- ^ Short name of a Game to be returned, serves as the unique identifier for the game
  }
  deriving (Generic, Show)

deriveJSON' ''CallbackQuery
