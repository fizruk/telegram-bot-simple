{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Telegram.Bot.API.InlineMode.InlineQueryResult where

import           Data.Aeson                      (FromJSON (..), ToJSON (..), Value (String))
import           Data.Hashable                   (Hashable)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.Types (Contact)
import           Telegram.Bot.API.InlineMode.InputMessageContent
import Telegram.Bot.API.Internal.TH (makeDefault)

-- | This object represents one result of an inline query
data InlineQueryResult = InlineQueryResult
  { inlineQueryResultType :: InlineQueryResultType -- ^ Type of the result
  , inlineQueryResultId :: InlineQueryResultId -- ^ Unique identifier for this result, 1-64 Bytes
  , inlineQueryResultTitle :: Maybe Text -- ^ Title of the result (only valid for "Article", "Photo", "Gif", "Mpeg4Gif", "Video", "Audio", "Voice", "Document", "Location", "Venue", "CachedPhoto", "CachedGif", "CachedMpeg4Gif", "CachedDocument", "CachedVideo", "CachedVoice" types of results)
  , inlineQueryResultInputMessageContent :: Maybe InputMessageContent
  , inlineQueryResultContact  :: Maybe Contact
  } deriving (Generic, Show)

newtype InlineQueryResultId = InlineQueryResultId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Hashable)

instance ToJSON InlineQueryResult where toJSON = gtoJSON
instance FromJSON InlineQueryResult where parseJSON = gparseJSON

-- | Type of inline query result
data InlineQueryResultType
  = InlineQueryResultCachedAudio
  | InlineQueryResultCachedDocument
  | InlineQueryResultCachedGif
  | InlineQueryResultCachedMpeg4Gif
  | InlineQueryResultCachedPhoto
  | InlineQueryResultCachedSticker
  | InlineQueryResultCachedVideo
  | InlineQueryResultCachedVoice
  | InlineQueryResultArticle
  | InlineQueryResultAudio
  | InlineQueryResultContact
  | InlineQueryResultGame
  | InlineQueryResultDocument
  | InlineQueryResultGif
  | InlineQueryResultLocation
  | InlineQueryResultMpeg4Gif
  | InlineQueryResultPhoto
  | InlineQueryResultVenue
  | InlineQueryResultVideo
  | InlineQueryResultVoice
  deriving (Eq, Show, Generic)

getType :: InlineQueryResultType -> Text
getType InlineQueryResultCachedAudio = "audio"
getType InlineQueryResultCachedDocument = "document"
getType InlineQueryResultCachedGif = "gif"
getType InlineQueryResultCachedMpeg4Gif = "mpeg4_gif"
getType InlineQueryResultCachedPhoto = "photo"
getType InlineQueryResultCachedSticker = "sticker"
getType InlineQueryResultCachedVideo = "video"
getType InlineQueryResultCachedVoice = "voice"
getType InlineQueryResultArticle = "article"
getType InlineQueryResultAudio = "audio"
getType InlineQueryResultContact = "contact"
getType InlineQueryResultGame = "game"
getType InlineQueryResultDocument = "document"
getType InlineQueryResultGif = "gif"
getType InlineQueryResultLocation = "location"
getType InlineQueryResultMpeg4Gif = "mpeg4_gif"
getType InlineQueryResultPhoto = "photo"
getType InlineQueryResultVenue = "venue"
getType InlineQueryResultVideo = "video"
getType InlineQueryResultVoice = "voice"

instance ToJSON InlineQueryResultType where
  toJSON = String . getType

instance FromJSON InlineQueryResultType where parseJSON = gparseJSON

makeDefault ''InlineQueryResult
