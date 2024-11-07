{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Types.ReactionType where

import Data.Aeson (FromJSON (..), ToJSON (..), KeyValue ((.=)), Value (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

import qualified Data.Text as Text

-- ** 'ReactionType'

-- | This object describes the type of a reaction. Currently, it can be one of
--
-- * 'ReactionTypeEmoji',
-- * 'ReactionTypeCustomEmoji'.
--
data ReactionType
  -- ^ The reaction is based on an emoji.
  = ReactionTypeEmoji
      { reactionTypeEmojiType  :: Text -- ^ Type of the reaction, always “emoji”.
      , reactionTypeEmojiEmoji :: Text -- ^ Reaction emoji. Currently, it can be one of "👍", "👎", "❤", "🔥", "🥰", "👏", "😁", "🤔", "🤯", "😱", "🤬", "😢", "🎉", "🤩", "🤮", "💩", "🙏", "👌", "🕊", "🤡", "🥱", "🥴", "😍", "🐳", "❤‍🔥", "🌚", "🌭", "💯", "🤣", "⚡", "🍌", "🏆", "💔", "🤨", "😐", "🍓", "🍾", "💋", "🖕", "😈", "😴", "😭", "🤓", "👻", "👨‍💻", "👀", "🎃", "🙈", "😇", "😨", "🤝", "✍", "🤗", "🫡", "🎅", "🎄", "☃", "💅", "🤪", "🗿", "🆒", "💘", "🙉", "🦄", "😘", "💊", "🙊", "😎", "👾", "🤷‍♂", "🤷", "🤷‍♀", "😡".
      }
  -- ^ The reaction is based on a custom emoji.
  | ReactionTypeCustomEmoji
      { reactionTypeCustomEmojiType :: Text -- ^ Type of the reaction, always “custom_emoji”.
      , reactionTypeCustomEmojiCustomEmojiId :: Text -- ^ Custom emoji identifier.
      }
  deriving Show

instance ToJSON ReactionType where
  toJSON = \case
    ReactionTypeEmoji _t e -> addJsonFields (Object mempty) (addType "emoji" ["emoji" .= e])
    ReactionTypeCustomEmoji _t cei ->
      addJsonFields (Object mempty) (addType "custom_emoji" ["custom_emoji_id" .= cei])

instance FromJSON ReactionType where
  parseJSON = withObject "ReactionType" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "emoji" -> ReactionTypeEmoji
      <$> o .: "type"
      <*> o .: "emoji"
    "custom_emoji" -> ReactionTypeCustomEmoji
      <$> o .: "type"
      <*> o .: "custom_emoji_id"
    t -> fail $ Text.unpack ("Unknown type: " <> t)
