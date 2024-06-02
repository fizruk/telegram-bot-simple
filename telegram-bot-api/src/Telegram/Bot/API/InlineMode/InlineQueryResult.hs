{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module Telegram.Bot.API.InlineMode.InlineQueryResult where

import           Data.Aeson
                 ( FromJSON (..), ToJSON (..), KeyValue ((.=)), Value (..)
                 , withObject, (.:), (.:?)
                 )
import           Data.Aeson.Types (Parser)
import           Data.Hashable                   (Hashable)
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.Types
import           Telegram.Bot.API.InlineMode.InputMessageContent
import           Telegram.Bot.API.Internal.TH (makeDefault)

import qualified Data.Text as Text

newtype InlineQueryResultId = InlineQueryResultId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON, Hashable)

data InlineQueryResultGeneric = InlineQueryResultGeneric
  { inlineQueryResultId :: InlineQueryResultId -- ^ Unique identifier for this result, 1-64 Bytes
  , inlineQueryResultTitle :: Maybe Text -- ^ Title of the result (only valid for "Article", "Photo", "Gif", "Mpeg4Gif", "Video", "Audio", "Voice", "Document", "Location", "Venue", "CachedPhoto", "CachedGif", "CachedMpeg4Gif", "CachedDocument", "CachedVideo", "CachedVoice" types of results)
  , inlineQueryResultCaption :: Maybe Text -- ^ Caption of the media to be sent, 0-1024 characters after entities parsing.
  , inlineQueryResultParseMode :: Maybe Text -- ^ Mode for parsing entities in the photo caption. See formatting options <https:\/\/core.telegram.org\/bots\/api#formatting-options> for more details.
  , inlineQueryResultCaptionEntities :: Maybe [MessageEntity] -- ^ List of special entities that appear in the caption, which can be specified instead of @parse_mode@.
  , inlineQueryResultShowCaptionAboveMedia :: Maybe Bool -- ^ Pass 'True', if the caption must be shown above the message media.
  , inlineQueryResultReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Inline keyboard attached to the message.
  , inlineQueryResultInputMessageContent :: Maybe InputMessageContent -- ^  Content of the message to be sent instead of the media.
  , inlineQueryResultDescription :: Maybe Text -- ^ Short description of the result.
  }
  deriving (Generic, Show)

instance ToJSON InlineQueryResultGeneric where toJSON = gtoJSON

instance FromJSON InlineQueryResultGeneric where parseJSON = gparseJSON

data InlineQueryResultGenericThumbnail = InlineQueryResultGenericThumbnail
  { inlineQueryResultGenericGeneric :: InlineQueryResultGeneric
  , inlineQueryResultGenericThumbnailUrl :: Maybe Text -- ^ URL of the thumbnail for the media.
  , inlineQueryResultGenericThumbnailMimeType :: Maybe Text -- ^ MIME type of the thumbnail, must be one of @image/jpeg@, @image/gif@, or @video/mp4@. Defaults to @image/jpeg@.
  , inlineQueryResultGenericThumbnailWidth :: Maybe Integer -- ^ Media width.
  , inlineQueryResultGenericThumbnailHeight :: Maybe Integer -- ^ Media height.
  }
  deriving (Generic, Show)

instance ToJSON InlineQueryResultGenericThumbnail where
  toJSON InlineQueryResultGenericThumbnail{..}
    = addJsonFields (toJSON inlineQueryResultGenericGeneric)
      [ "thumbnail_url" .= inlineQueryResultGenericThumbnailUrl
      , "thumbnail_mime_type" .= inlineQueryResultGenericThumbnailMimeType
      , "thumbnail_width" .= inlineQueryResultGenericThumbnailWidth
      , "thumbnail_height" .= inlineQueryResultGenericThumbnailHeight
      ]

instance FromJSON InlineQueryResultGenericThumbnail where
  parseJSON = withObject "InlineQueryResult" \o -> InlineQueryResultGenericThumbnail
    <$> parseJSON (Object o)
    <*> o .: "thumbnail_url"
    <*> o .: "thumbnail_mime_type"
    <*> o .: "thumbnail_width"
    <*> o .: "thumbnail_height"

-- | This object represents one result of an inline query
data InlineQueryResult
  = InlineQueryResultArticle
    { inlineQueryResultArticleGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultArticleUrl :: Maybe Text -- ^ URL of the result.
    , inlineQueryResultArticleHideUrl :: Maybe Bool -- ^ 'True' if you don't want the URL to be shown in the message.
    }
  | InlineQueryResultPhoto
    { inlineQueryResultPhotoGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultPhotoPhotoUrl :: Text -- ^ A valid URL of the photo. Photo must be in **JPEG** format. Photo size must not exceed 5MB.
    , inlineQueryResultPhotoPhotoWidth :: Maybe Integer -- ^ Width of the photo.
    , inlineQueryResultPhotoPhotoHeight :: Maybe Integer -- ^ Height of the photo.
    }
  | InlineQueryResultGif
    { inlineQueryResultGifGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultGifGifUrl :: Text -- ^ A valid URL for the GIF file. File size must not exceed 1MB.
    , inlineQueryResultGifGifWidth :: Maybe Integer -- ^ Width of the GIF.
    , inlineQueryResultGifGifHeight :: Maybe Integer -- ^ Height of the GIF.
    , inlineQueryResultGifGifDuration :: Maybe Integer -- ^ Duration of the GIF in seconds.
    }
  | InlineQueryResultMpeg4Gif
    { inlineQueryResultMpeg4GifGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultMpeg4GifMpeg4Url :: Text -- ^ A valid URL for the MPEG4 file. File size must not exceed 1MB.
    , inlineQueryResultMpeg4GifMpeg4Width :: Maybe Integer -- ^ Video width.
    , inlineQueryResultMpeg4GifMpeg4Height :: Maybe Integer -- ^ Video height.
    , inlineQueryResultMpeg4GifMpeg4Duration :: Maybe Integer -- ^ Video duration in seconds.
    }
  | InlineQueryResultVideo
    { inlineQueryResultVideoGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultVideoVideoUrl :: Text -- ^ A valid URL for the embedded video player or video file.
    , inlineQueryResultVideoMimeType :: Text -- ^ MIME type of the content of the video URL, @text/html@ or @video/mp4@.
    , inlineQueryResultVideoVideoWidth :: Maybe Integer -- ^ Video width.
    , inlineQueryResultVideoVideoHeight :: Maybe Integer -- ^ Video height.
    , inlineQueryResultVideoVideoDuration :: Maybe Integer -- ^ Video duration in seconds.
    }
  | InlineQueryResultAudio
    { inlineQueryResultAudioGeneric :: InlineQueryResultGeneric
    , inlineQueryResultAudioAudioUrl :: Text -- ^ A valid URL for the audio file.
    , inlineQueryResultAudioPerformer :: Maybe Text -- ^ Performer.
    , inlineQueryResultAudioAudioDuration :: Maybe Integer -- ^ Audio duration in seconds.
    }
  | InlineQueryResultVoice
    { inlineQueryResultVoiceGeneric :: InlineQueryResultGeneric
    , inlineQueryResultVoiceVoiceUrl :: Text -- ^ A valid URL for the voice recording.
    , inlineQueryResultVoiceVoiceDuration :: Maybe Integer -- ^ Recording duration in seconds.
    }
  | InlineQueryResultDocument
    { inlineQueryResultDocumentGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultDocumentDocumentUrl :: Text -- ^ A valid URL for the file.
    , inlineQueryResultDocumentMimeType :: Text -- ^ MIME type of the content of the file, either @application/pdf@ or @application/zip@.
    }
  | InlineQueryResultLocation
    { inlineQueryResultLocationGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultLocationLatitude :: Float -- ^ Location latitude in degrees.
    , inlineQueryResultLocationLongitude :: Float -- ^ Location longitude in degrees.
    , inlineQueryResultLocationHorizontalAccuracy :: Maybe Float -- ^ The radius of uncertainty for the location, measured in meters; 0-1500.
    , inlineQueryResultLocationLivePeriod :: Maybe Seconds -- ^ Period in seconds for which the location can be updated, should be between 60 and 86400.
    , inlineQueryResultLocationHeading :: Maybe Int -- ^ For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
    , inlineQueryResultLocationProximityAlertRadius :: Maybe Int -- ^ For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
    }
  | InlineQueryResultVenue
    { inlineQueryResultVenueGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultVenueLatitude :: Float -- ^ Latitude of the venue location in degrees.
    , inlineQueryResultVenueLongitude :: Float -- ^ Longitude of the venue location in degrees.
    , inlineQueryResultVenueAddress :: Text -- ^ Address of the venue.
    , inlineQueryResultVenueFoursquareId :: Maybe Text -- ^ Foursquare identifier of the venue if known.
    , inlineQueryResultVenueFoursquareType :: Maybe Text -- ^ Foursquare type of the venue, if known. (For example, @arts_entertainment/default@, @arts_entertainment/aquarium@ or @food/icecream@.)
    , inlineQueryResultVenueGooglePlaceId :: Maybe Text -- ^ Google Places identifier of the venue.
    , inlineQueryResultVenueGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See supported types <https:\/\/developers.google.com\/places\/web-service\/supported_types>.)
    }
  | InlineQueryResultContact
    { inlineQueryResultContactGeneric :: InlineQueryResultGenericThumbnail
    , inlineQueryResultContactPhoneNumber :: Text -- ^ Contact's phone number.
    , inlineQueryResultContactFirstName :: Text -- ^ Contact's first name.
    , inlineQueryResultContactLastName :: Maybe Text -- ^ Contact's last name.
    , inlineQueryResultContactVcard :: Maybe Text -- ^ Additional data about the contact in the form of a vCard <https:\/\/en.wikipedia.org\/wiki\/VCard>, 0-2048 bytes.
    }
  | InlineQueryResultGame
    { inlineQueryResultGameGeneric :: InlineQueryResultGeneric
    , inlineQueryResultGameGameShortName :: Text -- ^ Short name of the game.
    }
  | InlineQueryResultCachedPhoto
    { inlineQueryResultCachedPhotoGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedPhotoPhotoFileId :: FileId -- ^ A valid file identifier of the photo.
    }
  | InlineQueryResultCachedGif
    { inlineQueryResultCachedGifGeneric :: InlineQueryResultGeneric
    , iinlineQueryResultCachedGifGifFileId :: FileId -- ^ A valid file identifier for the GIF file.
    }
  | InlineQueryResultCachedMpeg4Gif
    { inlineQueryResultCachedMpeg4GifGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedMpeg4GifMpeg4FileId :: FileId -- ^ A valid file identifier for the MPEG4 file.
    }
  | InlineQueryResultCachedSticker
    { inlineQueryResultCachedStickerGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedStickerStickerFileId :: FileId -- ^ A valid file identifier of the sticker.
    }
  | InlineQueryResultCachedDocument
    { inlineQueryResultCachedDocumentGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedDocumentDocumentFileId :: FileId -- ^ A valid file identifier for the file.
    }
  | InlineQueryResultCachedVideo
    { inlineQueryResultCachedVideoGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedVideoVideoFileId :: FileId -- ^ A valid file identifier for the video file.
    }
  | InlineQueryResultCachedVoice
    { inlineQueryResultCachedVoiceGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedVoiceVoiceFileId :: FileId -- ^ A valid file identifier for the voice message.
    }
  | InlineQueryResultCachedAudio
    { inlineQueryResultCachedAudioGeneric :: InlineQueryResultGeneric
    , inlineQueryResultCachedAudioAudioFileId :: FileId -- ^ A valid file identifier for the audio file.
    }
  deriving (Generic, Show)

instance ToJSON InlineQueryResult where
  toJSON = \case
    InlineQueryResultArticle g url hideUrl ->
      addJsonFields (toJSON g)
        (addType "article"
        [ "url" .= url
        , "hide_url" .= hideUrl
        ])
    InlineQueryResultPhoto g photoUrl photoWidth photoHeight ->
      addJsonFields (toJSON g)
        (addType "photo"
        [ "photo_url" .= photoUrl
        , "photo_width" .= photoWidth
        , "photo_height" .= photoHeight
        ])
    InlineQueryResultGif g gifUrl gifWidth gifHeight gifDuration ->
      addJsonFields (toJSON g)
        (addType "gif"
        [ "gif_url" .= gifUrl
        , "gif_width" .= gifWidth
        , "gif_height" .= gifHeight
        , "gif_duration" .= gifDuration
        ])
    InlineQueryResultMpeg4Gif g mpeg4Url mpeg4Width mpeg4Height mpeg4Duration ->
      addJsonFields (toJSON g)
        (addType "mpeg4_gif"
        [ "mpeg4_url" .= mpeg4Url
        , "mpeg4_width" .= mpeg4Width
        , "mpeg4_height" .= mpeg4Height
        , "mpeg4_duration" .= mpeg4Duration
        ])
    InlineQueryResultVideo g videoUrl mimeType videoWidth videoHeight videoDuration ->
      addJsonFields (toJSON g)
        (addType "video"
        [ "video_url" .= videoUrl
        , "mime_type" .= mimeType
        , "video_width" .= videoWidth
        , "video_height" .= videoHeight
        , "video_duration" .= videoDuration
        ])
    InlineQueryResultAudio g audioUrl performer audioDuration ->
      addJsonFields (toJSON g)
        (addType "audio"
        [ "audio_url" .= audioUrl
        , "performer" .= performer
        , "audio_duration" .= audioDuration
        ])
    InlineQueryResultVoice g voiceUrl voiceDuration ->
      addJsonFields (toJSON g)
        (addType "voice"
        [ "voice_url" .= voiceUrl
        , "voice_duration" .= voiceDuration
        ])
    InlineQueryResultDocument g documentUrl mimeType ->
      addJsonFields (toJSON g)
        (addType "document"
        [ "document_url" .= documentUrl
        , "mime_type" .= mimeType
        ])
    InlineQueryResultLocation g latitude longitude horizontalAccuracy livePeriod heading proximityAlertRadius ->
      addJsonFields (toJSON g)
        (addType "location"
        [ "latitude" .= latitude
        , "longitude" .= longitude
        , "horizontal_accuracy" .= horizontalAccuracy
        , "live_period" .= livePeriod
        , "heading" .= heading
        , "proximity_alert_radius" .= proximityAlertRadius
        ])
    InlineQueryResultVenue g latitude longitude address foursquareId foursquareType googlePlaceId googlePlaceType ->
      addJsonFields (toJSON g)
        (addType "venue"
        [ "latitude" .= latitude
        , "longitude" .= longitude
        , "address" .= address
        , "foursquare_id" .= foursquareId
        , "foursquare_type" .= foursquareType
        , "google_place_id" .= googlePlaceId
        , "google_place_type" .= googlePlaceType
        ])
    InlineQueryResultContact g phoneNumber firstName lastName vcard ->
      addJsonFields (toJSON g)
        (addType "contact"
        [ "phone_number" .= phoneNumber
        , "first_name" .= firstName
        , "last_name" .= lastName
        , "vcard" .= vcard
        ])
    InlineQueryResultGame g gameShortName ->
      addJsonFields (toJSON g)
        (addType "game"
        [ "game_short_name" .= gameShortName
        ])
    InlineQueryResultCachedPhoto g photoFileId ->
      addJsonFields (toJSON g)
        (addType "photo"
        [ "photo_file_id" .= photoFileId
        ])
    InlineQueryResultCachedGif g gifFileId ->
      addJsonFields (toJSON g)
        (addType "gif"
        [ "gif_file_id" .= gifFileId
        ])
    InlineQueryResultCachedMpeg4Gif g mpeg4FileId ->
      addJsonFields (toJSON g)
        (addType "mpeg4_gif"
        [ "mpeg4_file_id" .= mpeg4FileId
        ])
    InlineQueryResultCachedSticker g stickerFileId ->
      addJsonFields (toJSON g)
        (addType "sticker"
        [ "sticker_file_id" .= stickerFileId
        ])
    InlineQueryResultCachedDocument g documentFileId ->
      addJsonFields (toJSON g)
        (addType "document"
        [ "document_file_id" .= documentFileId
        ])
    InlineQueryResultCachedVideo g videoFileId ->
      addJsonFields (toJSON g)
        (addType "video"
        [ "video_file_id" .= videoFileId
        ])
    InlineQueryResultCachedVoice g voiceFileId ->
      addJsonFields (toJSON g)
        (addType "voice"
        [ "voice_file_id" .= voiceFileId
        ])
    InlineQueryResultCachedAudio g audioFileId ->
      addJsonFields (toJSON g)
        (addType "audio"
        [ "audio_file_id" .= audioFileId
        ])

instance FromJSON InlineQueryResult where
  parseJSON = withObject "InlineQueryResult" \o ->
    (o .: "type" :: Parser Text) >>= \case
    "article" -> InlineQueryResultArticle
      <$> parseJSON (Object o)
      <*> o .:? "url"
      <*> o .:? "hide_url"
    "photo" -> parseFileId o "photo_file_id" >>= \case
      Nothing -> InlineQueryResultPhoto
        <$> parseJSON (Object o) -- generic thumbnail
        <*> o .: "photo_url"
        <*> o .:? "photo_width"
        <*> o .:? "photo_height"
      Just fileId -> InlineQueryResultCachedPhoto <$> parseJSON (Object o) <*> pure fileId
    "gif" -> parseFileId o "gif_file_id" >>= \case
      Nothing -> InlineQueryResultGif
        <$> parseJSON (Object o) -- generic thumbnail
        <*> o .: "gif_url"
        <*> o .:? "gif_width"
        <*> o .:? "gif_height"
        <*> o .:? "gif_duration"
      Just fileId -> InlineQueryResultCachedGif <$> parseJSON (Object o) <*> pure fileId
    "mpeg4_gif" -> parseFileId o "mpeg4_file_id" >>= \case
      Nothing -> InlineQueryResultMpeg4Gif
        <$> parseJSON (Object o) -- generic thumbnail
        <*> o .: "mpeg4_url"
        <*> o .:? "mpeg4_width"
        <*> o .:? "mpeg4_height"
        <*> o .:? "mpeg4_duration"
      Just fileId -> InlineQueryResultCachedMpeg4Gif
        <$> parseJSON (Object o)
        <*> pure fileId
    "video" -> parseFileId o "video_file_id" >>= \case
      Nothing -> InlineQueryResultVideo
        <$> parseJSON (Object o)
        <*> o .: "video_url"
        <*> o .: "mime_type"
        <*> o .:? "video_width"
        <*> o .:? "video_height"
        <*> o .:? "video_duration"
      Just fileId -> InlineQueryResultCachedVideo
        <$> parseJSON (Object o)
        <*> pure fileId
    "audio" -> parseFileId o "audio_file_id" >>= \case
      Nothing -> InlineQueryResultAudio
        <$> parseJSON (Object o)
        <*> o .: "audio_url"
        <*> o .:? "performer"
        <*> o .:? "duration"
      Just fileId -> InlineQueryResultCachedAudio <$> parseJSON (Object o) <*> pure fileId
    "voice" -> parseFileId o "voice_file_id" >>= \case
      Nothing -> InlineQueryResultVoice
        <$> parseJSON (Object o)
        <*> o .: "voice_url"
        <*> o .:? "voice_duration"
      Just fileId -> InlineQueryResultCachedVoice <$> parseJSON (Object o) <*> pure fileId
    "document" -> parseFileId o "document_file_id" >>= \case
      Nothing -> InlineQueryResultDocument
        <$> parseJSON (Object o)
        <*> o .: "document_url"
        <*> o .: "mime_type"
      Just fileId -> InlineQueryResultCachedDocument <$> parseJSON (Object o) <*> pure fileId
    "location" -> InlineQueryResultLocation
      <$> parseJSON (Object o)
      <*> o .: "latitude"
      <*> o .: "longitude"
      <*> o .:? "horizontal_accuracy"
      <*> o .:? "live_period"
      <*> o .:? "heading"
      <*> o .:? "proximity_alert_radius"
    "venue" -> InlineQueryResultVenue
      <$> parseJSON (Object o)
      <*> o .: "latitude"
      <*> o .: "longitude"
      <*> o .: "address"
      <*> o .:? "foursquare_id"
      <*> o .:? "foursquare_type"
      <*> o .:? "google_place_id"
      <*> o .:? "google_place_type"
    "contact" -> InlineQueryResultContact
      <$> parseJSON (Object o)
      <*> o .: "phone_number"
      <*> o .: "first_name"
      <*> o .:? "last_name"
      <*> o .:? "vcard"
    "game" -> InlineQueryResultGame
      <$> parseJSON (Object o)
      <*> o .: "game_short_name"
    t -> fail $ Text.unpack ("Unknown type: " <> t)
    where
      parseFileId o fileField = o .:? fileField :: Parser (Maybe FileId)
    
defInlineQueryResultArticle :: InlineQueryResultGenericThumbnail -> InlineQueryResult
defInlineQueryResultArticle g = InlineQueryResultArticle g Nothing Nothing

defInlineQueryResultPhotoUrl :: InlineQueryResultGenericThumbnail -> Text -> InlineQueryResult
defInlineQueryResultPhotoUrl g photoUrl = InlineQueryResultPhoto g photoUrl Nothing Nothing

defInlineQueryResultGif :: InlineQueryResultGenericThumbnail -> Text -> InlineQueryResult
defInlineQueryResultGif g gifUrl = InlineQueryResultGif g gifUrl Nothing Nothing Nothing

defInlineQueryResultMpeg4Gif :: InlineQueryResultGenericThumbnail -> Text -> InlineQueryResult
defInlineQueryResultMpeg4Gif g mpeg4Url = InlineQueryResultMpeg4Gif g mpeg4Url Nothing Nothing Nothing

defInlineQueryResultVideo :: InlineQueryResultGenericThumbnail -> Text -> Text -> InlineQueryResult
defInlineQueryResultVideo g videoUrl mimeType
  = InlineQueryResultVideo g videoUrl mimeType Nothing Nothing Nothing

defInlineQueryResultAudio :: InlineQueryResultGeneric -> Text -> InlineQueryResult
defInlineQueryResultAudio g audioUrl = InlineQueryResultAudio g audioUrl Nothing Nothing

defInlineQueryResultVoice :: InlineQueryResultGeneric -> Text -> InlineQueryResult
defInlineQueryResultVoice g voiceUrl = InlineQueryResultVoice g voiceUrl Nothing

defInlineQueryResultDocument :: InlineQueryResultGenericThumbnail -> Text -> Text -> InlineQueryResult
defInlineQueryResultDocument = InlineQueryResultDocument

defInlineQueryResultLocation :: InlineQueryResultGenericThumbnail -> Float -> Float -> InlineQueryResult
defInlineQueryResultLocation g lat lon
  = InlineQueryResultLocation g lat lon Nothing Nothing Nothing Nothing

defInlineQueryResultVenue :: InlineQueryResultGenericThumbnail -> Float -> Float -> Text -> InlineQueryResult
defInlineQueryResultVenue g lat lon address
  = InlineQueryResultVenue g lat lon address Nothing Nothing Nothing Nothing

defInlineQueryResultContact :: InlineQueryResultGenericThumbnail -> Text -> Text -> InlineQueryResult
defInlineQueryResultContact g phoneNumber firstName
  = InlineQueryResultContact g phoneNumber firstName Nothing Nothing

-- | This object represents a button to be shown above inline query results. You must use exactly one of the optional fields.
data InlineQueryResultsButton = InlineQueryResultsButton
  { inlineQueryResultsButtonText :: Text 
  , inlineQueryResultsButtonWebApp :: Maybe WebAppInfo
  , inlineQueryResultsButtonStartParameter :: Maybe Text -- ^ [Deep-linking](https://core.telegram.org/bots/features#deep-linking) parameter for the /start message sent to the bot when a user presses the button. 1-64 characters, only @A-Z@, @a-z@, @0-9@, @_@ and @-@ are allowed.
-- 
-- Example: An inline bot that sends YouTube videos can ask the user to connect the bot to their YouTube account to adapt search results accordingly. To do this, it displays a 'Connect your YouTube account' button above the results, or even before showing any. The user presses the button, switches to a private chat with the bot and, in doing so, passes a start parameter that instructs the bot to return an OAuth link. Once done, the bot can offer a [switch_inline](https://core.telegram.org/bots/api#inlinekeyboardmarkup) button so that the user can easily return to the chat where they wanted to use the bot's inline capabilities.
  }
  deriving Generic

instance ToJSON InlineQueryResultsButton where toJSON = gtoJSON

instance FromJSON InlineQueryResultsButton where parseJSON = gparseJSON

foldMap makeDefault
  [ ''InlineQueryResultGeneric
  , ''InlineQueryResultGenericThumbnail
  ]
