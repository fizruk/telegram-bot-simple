{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.MessageEntity where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** MessageEntity

-- | This object represents one special entity in a text message. For example, hashtags, usernames, URLs, etc.
data MessageEntity = MessageEntity
  { messageEntityType   :: MessageEntityType -- ^ Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames)
  , messageEntityOffset :: Int -- ^ Offset in UTF-16 code units to the start of the entity
  , messageEntityLength :: Int -- ^ Length of the entity in UTF-16 code units
  , messageEntityUrl    :: Maybe Text -- ^ For “text_link” only, url that will be opened after user taps on the text
  , messageEntityUser   :: Maybe User -- ^ For “text_mention” only, the mentioned user
  , messageEntityLanguage :: Maybe Text -- ^ For “pre” only, the programming language of the entity text.
  , messageEntityCustomEmojiId :: Maybe Text -- ^ For “custom_emoji” only, unique identifier of the custom emoji. Use @getCustomEmojiStickers@ to get full information about the sticker.
  }
  deriving (Generic, Show)

instance ToJSON   MessageEntity where toJSON = gtoJSON
instance FromJSON MessageEntity where parseJSON = gparseJSON


-- | Type of the entity. Can be mention (@username), hashtag, bot_command, url, email, bold (bold text), italic (italic text), underline (underlined text), strikethrough, code (monowidth string), pre (monowidth block), text_link (for clickable text URLs), text_mention (for users without usernames), cashtag, phone_number
data MessageEntityType
  = MessageEntityMention
  | MessageEntityHashtag
  | MessageEntityBotCommand
  | MessageEntityUrl
  | MessageEntityEmail
  | MessageEntityBold
  | MessageEntityItalic
  | MessageEntityUnderline -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_underline.html>
  | MessageEntityStrikethrough -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_strikethrough.html>
  | MessageEntitySpoiler
  | MessageEntityBlockquote
  | MessageEntityExpandableBlockquote -- ^ See <https://core.telegram.org/bots/api#may-28-2024>
  | MessageEntityCode
  | MessageEntityPre
  | MessageEntityTextLink
  | MessageEntityTextMention
  | MessageEntityCashtag -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_cashtag.html>.
  | MessageEntityPhoneNumber -- ^ See <https://core.telegram.org/tdlib/docs/classtd_1_1td__api_1_1text_entity_type_phone_number.html>.
  | MessageEntityCustomEmoji
  deriving (Eq, Show, Generic)

instance ToJSON   MessageEntityType where toJSON = gtoJSON
instance FromJSON MessageEntityType where parseJSON = gparseJSON
