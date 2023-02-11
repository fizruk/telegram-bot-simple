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
module Telegram.Bot.API.Methods.SendVoice where

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

-- ** 'sendVoice'

-- | Request parameters for 'sendVoice'.
data SendVoiceRequest = SendVoiceRequest
  { sendVoiceChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendVoiceMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendVoiceVoice :: InputFile -- ^ Audio file to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More info on Sending Files »
  , sendVoiceCaption :: Maybe Text -- ^ Voice message caption, 0-1024 characters after entities parsing
  , sendVoiceParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendVoiceCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendVoiceDuration :: Maybe Int -- ^ Duration of the voice message in seconds
  , sendVoiceDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVoiceProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendVoiceReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVoiceAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVoiceReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendVoiceRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendVoiceRequest where
  toMultipart SendVoiceRequest{..} =
    makeFile "voice" sendVoiceVoice $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendVoiceChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendVoiceMessageThreadId <&>
        \t -> Input "message_thread_id" (T.pack $ show t)
      , sendVoiceCaption <&>
        \t -> Input "caption" t
      , sendVoiceParseMode <&>
        \t -> Input "parse_mode" (TL.toStrict . TL.replace "\"" "" $ encodeToLazyText t)
      , sendVoiceCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendVoiceDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendVoiceProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendVoiceDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendVoiceReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendVoiceAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
      , sendVoiceReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendVoiceContent
  = "sendVoice"
  :> MultipartForm Tmp SendVoiceRequest
  :> Post '[JSON] (Response Message)

type SendVoiceLink
  = "sendVoice"
  :> ReqBody '[JSON] SendVoiceRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send audio files,
--   if you want Telegram clients to display
--   the file as a playable voice message. For
--   this to work, your audio must be in an .OGG
--   file encoded with OPUS (other formats may be
--   sent as Audio or Document).
--   On success, the sent Message is returned.
--   Bots can currently send voice messages of up
--   to 50 MB in size, this limit may be changed in the future.
sendVoice :: SendVoiceRequest ->  ClientM (Response Message)
sendVoice r = case sendVoiceVoice r of
  InputFile{} -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVoiceContent) (boundary, r)
  _ ->  client (Proxy @SendVoiceLink) r
