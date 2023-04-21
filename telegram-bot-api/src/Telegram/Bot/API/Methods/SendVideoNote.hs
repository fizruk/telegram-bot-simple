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
module Telegram.Bot.API.Methods.SendVideoNote where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bool
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))
import Data.Proxy
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
import Telegram.Bot.API.Internal.TH

-- ** 'sendVideoNote'

-- | Request parameters for 'sendVideoNote'.
data SendVideoNoteRequest = SendVideoNoteRequest
  { sendVideoNoteChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendVideoNoteMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendVideoNoteVideoNote :: InputFile -- ^ Video note to send. Pass a file_id as String to send a video note that exists on the Telegram servers (recommended) or upload a new video using multipart/form-data. More info on Sending Files ». Sending video notes by a URL is currently unsupported
  , sendVideoNoteDuration :: Maybe Int -- ^ Duration of sent video in seconds
  , sendVideoNoteLength :: Maybe Int -- ^ Video width and height, i.e. diameter of the video message
  , sendVideoNoteThumbnail :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendVideoNoteDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVideoNoteProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendVideoNoteReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVideoNoteAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVideoNoteReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendVideoNoteRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendVideoNoteRequest where
  toMultipart SendVideoNoteRequest{..} =
    maybe id (makeFile "thumbnail") sendVideoNoteThumbnail $
    makeFile "video_note" sendVideoNoteVideoNote $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendVideoNoteChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendVideoNoteMessageThreadId <&>
        \t -> Input "message_thread_id" (T.pack $ show t)
      , sendVideoNoteDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendVideoNoteProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendVideoNoteReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendVideoNoteAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
      , sendVideoNoteReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendVideoNoteContent
  = "sendVideoNote"
  :> MultipartForm Tmp SendVideoNoteRequest
  :> Post '[JSON] (Response Message)

type SendVideoNoteLink
  = "sendVideoNote"
  :> ReqBody '[JSON] SendVideoNoteRequest
  :> Post '[JSON] (Response Message)

-- | As of v.4.0, Telegram clients support rounded
--   square mp4 videos of up to 1 minute long. Use
--   this method to send video messages.
--   On success, the sent Message is returned.
sendVideoNote :: SendVideoNoteRequest ->  ClientM (Response Message)
sendVideoNote r = case (sendVideoNoteVideoNote r, sendVideoNoteThumbnail r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoNoteContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoNoteContent) (boundary, r)
  _ ->  client (Proxy @SendVideoNoteLink) r

makeDefault ''SendVideoNoteRequest
