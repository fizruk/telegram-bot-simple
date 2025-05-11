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
module Telegram.Bot.API.Methods.SendAnimation where

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

import qualified Data.Text.Lazy as TL

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Internal.TH

-- ** 'sendAnimation'

-- | Request parameters for 'sendAnimation'.
data SendAnimationRequest = SendAnimationRequest
  { sendAnimationBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendAnimationChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendAnimationMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendAnimationAnimation :: InputFile -- ^ Animation to send. Pass a file_id as String to send an animation that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an animation from the Internet, or upload a new animation using multipart/form-data. More info on Sending Files »
  , sendAnimationDuration :: Maybe Int -- ^ Duration of sent animation in seconds
  , sendAnimationWidth :: Maybe Int -- ^ Animation width
  , sendAnimationHeight :: Maybe Int -- ^ Animation height
  , sendAnimationThumbnail :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendAnimationCaption :: Maybe Text -- ^ Animation caption (may also be used when resending animation by file_id), 0-1024 characters after entities parsing
  , sendAnimationParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendAnimationCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of @parse_mode@.
  , sendAnimationShowCaptionAboveMedia :: Maybe Bool -- ^ Pass 'True', if the caption must be shown above the message media.
  , sendAnimationHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the animation needs to be covered with a spoiler animation.
  , sendAnimationDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendAnimationProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendAnimationMessageEffectId :: Maybe Text -- ^ Unique identifier of the message effect to be added to the message; for private chats only.
  , sendAnimationReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendAnimationReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendAnimationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendAnimationRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendAnimationRequest where
  toMultipart SendAnimationRequest{..} =
    maybe id (makeFile "thumbnail") sendAnimationThumbnail $
    makeFile "animation" sendAnimationAnimation $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendAnimationChatId of
          SomeChatId (ChatId chat_id) -> showText chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendAnimationMessageThreadId <&>
        \t -> Input "message_thread_id" (showText t)
      , sendAnimationCaption <&>
        \t -> Input "caption" t
      , sendAnimationParseMode <&>
        \t -> Input "parse_mode" (TL.toStrict . TL.replace "\"" "" $ encodeToLazyText t)
      , sendAnimationCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationHasSpoiler <&>
        \t -> Input "has_spoiler" (bool "false" "true" t)
      , sendAnimationDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationWidth <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationHeight <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendAnimationProtectContent <&>
        \t -> Input "protect_content" (bool "false" "true" t)
      , sendAnimationReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationReplyParameters <&>
        \t -> Input "reply_parameters" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationReplyMarkup <&>
        \t -> Input "reply_markup" (TL.toStrict $ encodeToLazyText t)
      ]

type SendAnimationContent
  = "sendAnimation"
  :> MultipartForm Tmp SendAnimationRequest
  :> Post '[JSON] (Response Message)

type SendAnimationLink
  = "sendAnimation"
  :> ReqBody '[JSON] SendAnimationRequest
  :> Post '[JSON] (Response Message)


-- | Use this method to send animation files
--   (GIF or H.264/MPEG-4 AVC video without sound).
--   On success, the sent Message is returned. Bots
--   can currently send animation files of up to 50
--   MB in size, this limit may be changed in the future.
sendAnimation :: SendAnimationRequest ->  ClientM (Response Message)
sendAnimation r = case (sendAnimationAnimation r, sendAnimationThumbnail r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAnimationContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAnimationContent) (boundary, r)
  _ ->  client (Proxy @SendAnimationLink) r

makeDefault ''SendAnimationRequest
