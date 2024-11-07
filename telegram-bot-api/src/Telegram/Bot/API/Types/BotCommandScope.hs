{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.BotCommandScope where

import Data.Aeson (KeyValue ((.=)), FromJSON (..), ToJSON (..), object, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import qualified Data.Text as Text

import Telegram.Bot.API.Types.Common

data BotCommandScope
  = BotCommandScopeDefault -- ^ Represents the default scope of bot commands. Default commands are used if no commands with a narrower scope are specified for the user.
  | BotCommandScopeAllPrivateChats -- ^ Represents the scope of bot commands, covering all private chats.
  | BotCommandScopeAllGroupChats -- ^ Represents the scope of bot commands, covering all group and supergroup chats.
  | BotCommandScopeAllChatAdministrators -- ^ Represents the scope of bot commands, covering all group and supergroup chat administrators.
  | BotCommandScopeChat SomeChatId -- ^ Represents the scope of bot commands, covering a specific chat.
  | BotCommandScopeChatAdministrators SomeChatId -- ^ Represents the scope of bot commands, covering all administrators of a specific group or supergroup chat.
  | BotCommandScopeChatMember SomeChatId UserId -- ^ Represents the scope of bot commands, covering a specific member of a group or supergroup chat.

instance ToJSON BotCommandScope where
  toJSON = \case
    BotCommandScopeDefault ->
      object $ addType "default" []
    BotCommandScopeAllPrivateChats ->
      object $ addType "all_private_chats" []
    BotCommandScopeAllGroupChats ->
      object $ addType "all_group_chats" []
    BotCommandScopeAllChatAdministrators ->
      object $ addType "all_chat_administrators" []
    BotCommandScopeChat sci ->
      object $ addType "chat" ["chat_id" .= sci]
    BotCommandScopeChatAdministrators sci ->
      object $ addType "chat_administrators" ["chat_id" .= sci]
    BotCommandScopeChatMember sci ui ->
      object $ addType "chat_member" ["chat_id" .= sci, "user_id" .= ui]

instance FromJSON BotCommandScope where
  parseJSON = withObject "BotCommandScope" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "default" ->                pure BotCommandScopeDefault
    "all_private_chats" ->      pure BotCommandScopeAllPrivateChats
    "all_group_chats" ->        pure BotCommandScopeAllGroupChats
    "all_chat_administrators"-> pure BotCommandScopeAllChatAdministrators
    "chat" ->                        BotCommandScopeChat <$> o .: "chat_id"
    "chat_administrators"->          BotCommandScopeChatAdministrators <$> o .: "chat_id"
    "chat_member"->                  BotCommandScopeChatMember <$> o .: "chat_id" <*> o .: "user_id"
    t -> fail $ Text.unpack ("Unknown type: " <> t)
