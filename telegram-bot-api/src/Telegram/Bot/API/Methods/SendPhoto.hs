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
module Telegram.Bot.API.Methods.SendPhoto where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..))
import Data.Aeson.Text (encodeToLazyText)
import Data.Bool
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Multipart.API
import Servant.Multipart.Client
import System.FilePath
import Servant.Client hiding (Response)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Types.SomeReplyMarkup
import Telegram.Bot.API.Internal.TH

-- * Available methods

-- ** 'sendPhoto'
type SendPhotoContent
  = "sendPhoto"
  :> MultipartForm Tmp SendPhotoRequest
  :> Post '[JSON] (Response Message)

type SendPhotoLink
  = "sendPhoto"
  :> ReqBody '[JSON] SendPhotoRequest
  :> Post '[JSON] (Response Message)

newtype PhotoFile = MakePhotoFile InputFile
  deriving newtype ToJSON

pattern PhotoFileId :: FileId -> PhotoFile
pattern PhotoFileId x = MakePhotoFile (InputFileId x)

pattern PhotoUrl :: Text -> PhotoFile
pattern PhotoUrl x = MakePhotoFile (FileUrl x)

pattern PhotoFile :: FilePath -> ContentType -> PhotoFile
pattern PhotoFile x y = MakePhotoFile (InputFile x y)


-- | Request parameters for 'sendPhoto'
data SendPhotoRequest = SendPhotoRequest
  { sendPhotoBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendPhotoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendPhotoMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendPhotoPhoto :: PhotoFile -- ^ Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data
  , sendPhotoThumb :: Maybe FilePath -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>
  , sendPhotoCaption :: Maybe Text -- ^ Photo caption (may also be used when resending Photos by file_id), 0-1024 characters after entities parsing
  , sendPhotoParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendPhotoCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of /parse_mode/.
  , sendPhotoShowCaptionAboveMedia :: Maybe Bool -- ^ Pass 'True', if the caption must be shown above the message media.
  , sendPhotoHasSpoiler :: Maybe Bool -- ^ Pass 'True' if the photo needs to be covered with a spoiler animation.
  , sendPhotoDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendPhotoProtectContent      :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendPhotoMessageEffectId :: Maybe Text -- ^ Unique identifier of the message effect to be added to the message; for private chats only.
  , sendPhotoReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendPhotoReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendPhotoReplyMarkup :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToMultipart Tmp SendPhotoRequest where
  toMultipart SendPhotoRequest{..} = MultipartData fields files where
    fields =
      [ Input "photo" $ T.pack $ "attach://file"
      , Input "chat_id" $ case sendPhotoChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <>
      (   (maybe id (\t -> ((Input "message_thread_id" (T.pack $ show t)):)) sendPhotoMessageThreadId)
        $ (maybe id (\_ -> ((Input "thumb" "attach://thumb"):)) sendPhotoThumb)
        $ (maybe id (\t -> ((Input "caption" t):)) sendPhotoCaption)
        $ (maybe id (\t -> ((Input "parse_mode" (TL.toStrict . TL.replace "\"" "" $ encodeToLazyText t)):)) sendPhotoParseMode)
        $ (maybe id (\t -> ((Input "caption_entities" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoCaptionEntities)
        $ (maybe id (\t -> ((Input "has_spoiler" (bool "false" "true" t)):)) sendPhotoHasSpoiler)
        $ (maybe id (\t -> ((Input "disable_notification" (bool "false" "true" t)):)) sendPhotoDisableNotification)
        $ (maybe id (\t -> ((Input "protect_content" (bool "false" "true" t)):)) sendPhotoProtectContent)
        $ (maybe id (\t -> ((Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoReplyToMessageId)
        $ (maybe id (\t -> ((Input "reply_parameters" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoReplyParameters)
        $ (maybe id (\t -> ((Input "reply_markup" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoReplyMarkup)
        [])
    files
      = (FileData "file" (T.pack $ takeFileName path) ct path)
      : maybe [] (\t -> [FileData "thumb" (T.pack $ takeFileName t) "image/jpeg" t]) sendPhotoThumb

    PhotoFile path ct = sendPhotoPhoto

instance ToJSON SendPhotoRequest where toJSON = gtoJSON

-- | Use this method to send photos.
-- On success, the sent 'Message' is returned.
--
-- <https:\/\/core.telegram.org\/bots\/api#sendphoto>
sendPhoto :: SendPhotoRequest -> ClientM (Response Message)
sendPhoto r = do
  case sendPhotoPhoto r of
    PhotoFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SendPhotoContent) (boundary, r)
    _ -> client (Proxy @SendPhotoLink) r

makeDefault ''SendPhotoRequest
