{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendVideo where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bool
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Multipart.API
import Servant.Multipart.Client
import Servant.Client hiding (Response)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Internal.TH

-- ** 'sendVideo'

-- | Request parameters for 'sendVideo'.
data SendVideoRequest = SendVideoRequest
  { sendVideoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendVideoMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendVideoVideo :: InputFile -- ^ Video to send. Pass a file_id as String to send an video that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a video from the Internet, or upload a new video using multipart/form-data. More info on Sending Files »
  , sendVideoDuration :: Maybe Int -- ^ Duration of sent video in seconds
  , sendVideoWidth :: Maybe Int -- ^ Video width
  , sendVideoHeight :: Maybe Int -- ^ Video height
  , sendVideoThumbnail :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendVideoCaption :: Maybe Text -- ^ Video caption (may also be used when resending videos by file_id), 0-1024 characters after entities parsing
  , sendVideoParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendVideoCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendVideoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the video needs to be covered with a spoiler animation.
  , sendVideoSupportsStreaming :: Maybe Bool -- ^ Pass True, if the uploaded video is suitable for streaming
  , sendVideoDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVideoProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendVideoReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVideoAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVideoReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendVideoRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendVideoRequest where
  toMultipart SendVideoRequest{..} =
    maybe id (makeFile "thumbnail") sendVideoThumbnail $
    makeFile "video" sendVideoVideo $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendVideoChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendVideoMessageThreadId <&>
        \t -> Input "message_thread_id" (T.pack $ show t)
      , sendVideoCaption <&>
        \t -> Input "caption" t
      , sendVideoParseMode <&>
        \t -> Input "parse_mode" (TL.toStrict . TL.replace "\"" "" $ encodeToLazyText t)
      , sendVideoCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendVideoHasSpoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      , sendVideoDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendVideoWidth <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , sendVideoHeight <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , sendVideoDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendVideoSupportsStreaming <&>
        \t -> Input "supports_streaming" (bool "false" "true" t)
      , sendVideoProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendVideoReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendVideoAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
      , sendVideoReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendVideoContent
  = "sendVideo"
  :> MultipartForm Tmp SendVideoRequest
  :> Post '[JSON] (Response Message)

type SendVideoLink
  = "sendVideo"
  :> ReqBody '[JSON] SendVideoRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send video files,
--   Telegram clients support mp4 videos
--   (other formats may be sent as Document).
--   On success, the sent Message is returned.
--   Bots can currently send video files of up
--   to 50 MB in size, this limit may be changed in the future.
sendVideo :: SendVideoRequest ->  ClientM (Response Message)
sendVideo r = case (sendVideoVideo r, sendVideoThumbnail r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoContent) (boundary, r)
  _ ->  client (Proxy @SendVideoLink) r

makeDefault ''SendVideoRequest
