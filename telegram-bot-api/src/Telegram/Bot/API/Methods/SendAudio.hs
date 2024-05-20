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
module Telegram.Bot.API.Methods.SendAudio where

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

-- ** 'sendAudio'

-- | Request parameters for 'sendAudio'.
data SendAudioRequest = SendAudioRequest
  { sendAudioBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendAudioChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendAudioMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendAudioAudio :: InputFile -- ^ Audio to send. Pass a file_id as String to send an audio that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a audio from the Internet, or upload a new audio using multipart/form-data. More info on Sending Files »
  , sendAudioDuration :: Maybe Int -- ^ Duration of sent audio in seconds
  , sendAudioPerformer :: Maybe Text -- ^ Performer
  , sendAudioTitle :: Maybe Text -- ^ Track name
  , sendAudioThumbnail :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendAudioCaption :: Maybe Text -- ^ Audio caption (may also be used when resending audios by file_id), 0-1024 characters after entities parsing
  , sendAudioParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendAudioCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendAudioDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendAudioProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendAudioReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendAudioReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendAudioReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendAudioRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendAudioRequest where
  toMultipart SendAudioRequest{..} =
    maybe id (makeFile "Thumbnail") sendAudioThumbnail $
    makeFile "audio" sendAudioAudio $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendAudioChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendAudioMessageThreadId <&>
        \t -> Input "message_thread_id" (T.pack $ show t)
      , sendAudioCaption <&>
        \t -> Input "caption" t
      , sendAudioParseMode <&>
        \t -> Input "parse_mode" (TL.toStrict . TL.replace "\"" "" $ encodeToLazyText t)
      , sendAudioCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendAudioDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendAudioPerformer <&>
        \t -> Input "performer" t
      , sendAudioTitle <&>
        \t -> Input "title" (TL.toStrict $ encodeToLazyText t)
      , sendAudioDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendAudioProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendAudioReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendAudioReplyParameters <&>
        \t -> Input "reply_parameters" (TL.toStrict $ encodeToLazyText t)
      , sendAudioReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendAudioContent
  = "sendAudio"
  :> MultipartForm Tmp SendAudioRequest
  :> Post '[JSON] (Response Message)

type SendAudioLink
  = "sendAudio"
  :> ReqBody '[JSON] SendAudioRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send audio files, if
--   you want Telegram clients to display them
--   in the music player. Your audio must be in
--   the .MP3 or .M4A format. On success, the sent
--   Message is returned. Bots can currently send
--   audio files of up to 50 MB in size, this limit
--   may be changed in the future.
--
--   For sending voice messages, use the sendVoice method instead.
sendAudio :: SendAudioRequest ->  ClientM (Response Message)
sendAudio r = case (sendAudioAudio r, sendAudioThumbnail r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAudioContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAudioContent) (boundary, r)
  _ ->  client (Proxy @SendAudioLink) r

makeDefault ''SendAudioRequest
