{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.MessageOrigin where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), (.=), (.:), (.:?), withObject)
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import qualified Data.Text as Text

import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'MessageOrigin'

-- | This object describes the origin of a message. It can be one of
--
-- * MessageOriginUser
-- * MessageOriginHiddenUser
-- * MessageOriginChat
-- * MessageOriginChannel
--
data MessageOrigin
  -- | The message was originally sent by a known user.
  = MessageOriginUser
      { messageOriginUserType :: Text -- ^ Type of the message origin, always “user”.
      , messageOriginUserDate :: POSIXTime -- ^ Date the message was sent originally in Unix time.
      , messageOriginUserSenderUser :: User -- ^ User that sent the message originally.
      }
  -- | The message was originally sent by an unknown user.
  | MessageOriginHiddenUser
      { messageOriginHiddenUserType :: Text -- ^ Type of the message origin, always “hidden_user”.
      , messageOriginHiddenUserDate :: POSIXTime -- ^ Date the message was sent originally in Unix time.
      , messageOriginHiddenUserSenderUserName :: Text -- ^ Name of the user that sent the message originally.
      }
  -- | The message was originally sent on behalf of a chat to a group chat.
  | MessageOriginChat
      { messageOriginChatType :: Text -- ^ Type of the message origin, always “chat”.
      , messageOriginChatDate :: POSIXTime -- ^ Date the message was sent originally in Unix time.
      , messageOriginChatSenderChat :: Chat -- ^ Chat that sent the message originally.
      , messageOriginChatAuthorSignature :: Maybe Text -- ^ For messages originally sent by an anonymous chat administrator, original message author signature.
      }
  -- | The message was originally sent to a channel chat.
  | MessageOriginChannel
      { messageOriginChannelType :: Text -- ^ Type of the message origin, always “channel”.
      , messageOriginChannelDate :: POSIXTime -- ^ Date the message was sent originally in Unix time.
      , messageOriginChannelChat :: Chat -- ^ Channel chat to which the message was originally sent.
      , messageOriginChannelMessageId :: MessageId -- ^ Unique message identifier inside the chat.
      , messageOriginChannelAuthorSignature :: Maybe Text -- ^ Signature of the original post author.
      }
  deriving (Generic, Show)

instance ToJSON   MessageOrigin where
  toJSON = \case
    MessageOriginUser _t d su -> addJsonFields
      (Object mempty)
      (addType "user" [ "date" .= d, "sender_user" .= su ])
    MessageOriginHiddenUser _t d sun -> addJsonFields
      (Object mempty)
      (addType "hidden_user" [ "date" .= d, "sender_user_name" .= sun ])
    MessageOriginChat _t d sc as -> addJsonFields
      (Object mempty)
      (addType "chat" [ "date" .= d, "sender_chat" .= sc, "author_signature" .= as ])
    MessageOriginChannel _t d c m as -> addJsonFields
      (Object mempty)
      (addType "channel" [ "date" .= d, "chat" .= c, "message_id" .= m, "author_signature" .= as])

instance FromJSON MessageOrigin where
  parseJSON = withObject "MessageOrigin" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "user" -> MessageOriginUser
      <$> o .: "type"
      <*> o .: "date"
      <*> o .: "sender_user"
    "hidden_user" -> MessageOriginHiddenUser
      <$> o .: "type"
      <*> o .: "date"
      <*> o .: "sender_user_name"
    "chat" -> MessageOriginChat
      <$> o .: "type"
      <*> o .: "date"
      <*> o .: "sender_chat"
      <*> o .:? "author_signature"
    "channel" -> MessageOriginChannel
      <$> o .: "type"
      <*> o .: "date"
      <*> o .: "chat"
      <*> o .: "message_id"
      <*> o .:? "author_signature"
    t -> fail $ Text.unpack ("Unknown type: " <> t)

