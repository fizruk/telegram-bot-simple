{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ReplyParameters where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Internal.Utils

-- ** 'ReplyParameters'

-- | Describes reply parameters for the message that is being sent.
data ReplyParameters = ReplyParameters
  { replyParametersMessageId :: MessageId -- ^ Identifier of the message that will be replied to in the current chat, or in the chat @chat_id@ if it is specified.
  , replyParametersChatId :: Maybe SomeChatId -- ^ f the message to be replied to is from a different chat, unique identifier for the chat or username of the channel (in the format \@channelusername).
  , replyParametersAllowSendingWithoutReply :: Maybe Bool -- ^ Pass 'True' if the message should be sent even if the specified message to be replied to is not found; can be used only for replies in the same chat and forum topic.
  , replyParametersQuote :: Maybe Text -- ^ Quoted part of the message to be replied to; 0-1024 characters after entities parsing. The quote must be an exact substring of the message to be replied to, including @bold@, @italic@, @underline@, @strikethrough@, @spoiler@, and @custom_emoji@ entities. The message will fail to send if the quote isn't found in the original message.
  , replyParametersQuoteParseMode :: Maybe ParseMode -- ^ Mode for parsing entities in the quote. See formatting options for more details.
  , replyParametersQuoteEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the quote. It can be specified instead of @quote_parse_mode@.
  , replyParametersQuotePosition :: Maybe Int -- ^ Position of the quote in the original message in UTF-16 code units.
  } deriving (Generic, Show)

instance ToJSON   ReplyParameters where toJSON = gtoJSON
instance FromJSON ReplyParameters where parseJSON = gparseJSON
