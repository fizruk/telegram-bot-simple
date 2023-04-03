{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Bot.API.Types.InputMedia where

import Data.Aeson (ToJSON (..), KeyValue ((.=)))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Servant.Multipart.API
import System.FilePath

import qualified Data.Text.Lazy as TL

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Internal.Utils

-- ** 'InputMedia'

-- | Generic fields for all InputMedia structures
data InputMediaGeneric = InputMediaGeneric
  { inputMediaGenericMedia :: InputFile -- ^ File to send. Pass a file_id to send a file that exists on the Telegram servers (recommended), pass an HTTP URL for Telegram to get a file from the Internet, or pass “attach://<file_attach_name>” to upload a new one using multipart/form-data under <file_attach_name> name.
  , inputMediaGenericCaption :: Maybe Text -- ^ Caption of the photo to be sent, 0-1024 characters after entities parsing.
  , inputMediaGenericParseMode :: Maybe Text -- ^ Mode for parsing entities in the photo caption. See formatting options <https:\/\/core.telegram.org\/bots\/api#formatting-options> for more details.
  , inputMediaGenericCaptionEntities :: Maybe [MessageEntity] -- ^ List of special entities that appear in the caption, which can be specified instead of parse_mode.
  }
  deriving Generic

instance ToJSON InputMediaGeneric where toJSON = gtoJSON

instance ToMultipart Tmp InputMediaGeneric where
  toMultipart InputMediaGeneric{..} = makeFile "media" inputMediaGenericMedia (MultipartData fields []) where
    fields = catMaybes
      [ inputMediaGenericCaption <&>
        \t -> Input "caption" t
      , inputMediaGenericParseMode <&>
        \t -> Input "parse_mode" t
      , inputMediaGenericCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      ]

data InputMediaGenericThumb = InputMediaGenericThumb
  { inputMediaGenericGeneric :: InputMediaGeneric
  , inputMediaGenericThumbnail :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. 
  }

instance ToJSON InputMediaGenericThumb where
  toJSON InputMediaGenericThumb{..}
    = addJsonFields (toJSON inputMediaGenericGeneric)
      ["thumbnail" .= inputMediaGenericThumbnail]

instance ToMultipart Tmp InputMediaGenericThumb where
  toMultipart = \case
    InputMediaGenericThumb generic Nothing -> toMultipart generic
    InputMediaGenericThumb generic (Just thumb) -> makeFile "thumbnail" thumb (toMultipart generic)


data InputMedia
  = InputMediaPhoto -- ^ Represents a photo to be sent.
    { inputMediaPhotoGeneric :: InputMediaGeneric
    , inputMediaPhotoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaVideo -- ^ Represents a video to be sent.
    { inputMediaVideoGeneric :: InputMediaGenericThumb
    , inputMediaVideoWidth :: Maybe Integer -- ^ Video width
    , inputMediaVideoHeight :: Maybe Integer -- ^ Video height
    , inputMediaVideoDuration :: Maybe Integer -- ^ Video duration in seconds
    , inputMediaVideoSupportsStreaming :: Maybe Bool -- ^ Pass 'True', if the uploaded video is suitable for streaming.
    , inputMediaVideoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaAnimation -- ^ Represents an animation file (GIF or H.264/MPEG-4 AVC video without sound) to be sent.
    { inputMediaAnimationGeneric :: InputMediaGenericThumb
    , inputMediaAnimationWidth :: Maybe Integer -- ^ Animation width
    , inputMediaAnimationHeight :: Maybe Integer -- ^ Animation height
    , inputMediaAnimationDuration :: Maybe Integer -- ^ Animation duration in seconds
    , inputMediaAnimationHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
    }
  | InputMediaAudio -- ^ Represents an audio file to be treated as music to be sent.
    { inputMediaAudioGeneric :: InputMediaGenericThumb
    , inputMediaAudioDuration :: Maybe Integer -- ^ Duration of the audio in seconds
    , inputMediaAudioPerformer :: Maybe Text -- ^ Performer of the audio
    , inputMediaAudioTitle :: Maybe Text -- ^ Title of the audio
    }
  | InputMediaDocument -- ^ Represents a general file to be sent.
    { inputMediaDocumentGeneric :: InputMediaGenericThumb
    , inputMediaDocumentDisableContentTypeDetection :: Maybe Bool -- ^ Disables automatic server-side content type detection for files uploaded using multipart/form-data. Always True, if the document is sent as part of an album.
    }

instance ToJSON InputMedia where
  toJSON = \case
    InputMediaPhoto img spoiler ->
      addJsonFields (toJSON img) (addType "photo" [ "has_spoiler" .= spoiler])
    InputMediaVideo imgt width height duration streaming spoiler ->
      addJsonFields (toJSON imgt)
                (addType "video"
                [ "width" .= width
                , "height" .= height
                , "duration" .= duration
                , "support_streaming" .= streaming
                , "has_spoiler" .= spoiler
                ])
    InputMediaAnimation imgt width height duration spoiler ->
      addJsonFields (toJSON imgt)
                (addType "animation"
                [ "width" .= width
                , "height" .= height
                , "duration" .= duration
                , "has_spoiler" .= spoiler
                ])
    InputMediaAudio imgt duration performer title ->
      addJsonFields (toJSON imgt)
                (addType "audio"
                [ "duration" .= duration
                , "performer" .= performer
                , "title" .= title
                ])
    InputMediaDocument imgt dctd ->
      addJsonFields (toJSON imgt)
                (addType "document" ["disable_content_type_detection" .= dctd])



instance ToMultipart Tmp InputMedia where
  toMultipart = let
    in \case
    InputMediaPhoto img spoiler ->
      addMultipartFields
      (Input "type" "photo"
       : catMaybes
        [ spoiler <&>
          \t -> Input "has_spoiler" (bool "false" "true" t)
        ]) (toMultipart img)
    InputMediaVideo imgt width height duration streaming spoiler ->
      addMultipartFields
      (Input "type" "video"
      : catMaybes 
      [ width <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , height <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , streaming <&>
        \t -> Input "support_streaming" (bool "false" "true" t)
      , spoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      ]) (toMultipart imgt)
    InputMediaAnimation imgt width height duration spoiler ->
      addMultipartFields
      (Input "type" "animation"
      : catMaybes 
      [ width <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , height <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , spoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      ]) (toMultipart imgt)
    InputMediaAudio imgt duration performer title ->
      addMultipartFields
      (Input "type" "audio"
      : catMaybes 
      [ duration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , performer <&>
        \t -> Input "performer" t
      , title <&>
        \t -> Input "title" t
      ]) (toMultipart imgt)
    InputMediaDocument imgt dctd ->
      addMultipartFields
      (Input "type" "document"
      : catMaybes 
      [ dctd <&> 
         \t -> Input "disable_content_type_detection" (bool "false" "true" t)
      ]) (toMultipart imgt)

type ContentType = Text

data InputFile
  = InputFileId FileId
  | FileUrl Text
  | InputFile FilePath ContentType

instance ToJSON InputFile where
  toJSON (InputFileId i) = toJSON i
  toJSON (FileUrl t) = toJSON t
  toJSON (InputFile f _) = toJSON ("attach://" <> pack (takeFileName f))


-- | Multipart file helper
makeFile :: Text -> InputFile ->  MultipartData Tmp ->  MultipartData Tmp
makeFile name (InputFile path ct) (MultipartData fields files) = 
  MultipartData 
    (Input name ("attach://" <> name) : fields) 
    (FileData name (pack $ takeFileName path) ct path : files)

makeFile name file (MultipartData fields files) = 
  MultipartData 
    (Input name (TL.toStrict $ encodeToLazyText file) : fields) 
    files
