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
module Telegram.Bot.API.Methods.SendDocument where

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

-- ** 'sendDocument'

type SendDocumentContent
  = "sendDocument"
  :> MultipartForm Tmp SendDocumentRequest
  :> Post '[JSON] (Response Message)

type SendDocumentLink
  = "sendDocument"
  :> ReqBody '[JSON] SendDocumentRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
--
-- <https:\/\/core.telegram.org\/bots\/api#senddocument>
sendDocument :: SendDocumentRequest -> ClientM (Response Message)
sendDocument r = do
  case sendDocumentDocument r of
    DocumentFile{} -> do
      boundary <- liftIO genBoundary
      client (Proxy @SendDocumentContent) (boundary, r)
    _ -> client (Proxy @SendDocumentLink) r

-- | Request parameters for 'sendDocument'
data SendDocumentRequest = SendDocumentRequest
  { sendDocumentBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendDocumentChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendDocumentMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendDocumentDocument :: DocumentFile -- ^ Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data
  , sendDocumentThumbnail :: Maybe FilePath -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>
  , sendDocumentCaption :: Maybe Text -- ^ Document caption (may also be used when resending documents by file_id), 0-1024 characters after entities parsing
  , sendDocumentParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendDocumentCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of /parse_mode/.
  , sendDocumentDisableContentTypeDetection :: Maybe Bool -- ^ Disables automatic server-side content type detection for files uploaded using @multipart/form-data@.
  , sendDocumentDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendDocumentProtectContent      :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendDocumentReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendDocumentReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendDocumentReplyMarkup :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic


newtype DocumentFile = MakeDocumentFile InputFile
  deriving newtype ToJSON

pattern DocumentFileId :: FileId -> DocumentFile
pattern DocumentFileId x = MakeDocumentFile (InputFileId x)

pattern DocumentUrl :: Text -> DocumentFile
pattern DocumentUrl x = MakeDocumentFile (FileUrl x)

pattern DocumentFile :: FilePath -> ContentType -> DocumentFile
pattern DocumentFile x y = MakeDocumentFile (InputFile x y)


instance ToMultipart Tmp SendDocumentRequest where
  toMultipart SendDocumentRequest{..} = MultipartData fields files where
    fields =
      [ Input "document" $ T.pack $ "attach://file"
      , Input "chat_id" $ case sendDocumentChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <>
      (   (maybe id (\t -> ((Input "message_thread_id") (T.pack $ show t):)) sendDocumentMessageThreadId)
        $ (maybe id (\_ -> ((Input "thumbnail" "attach://thumbnail"):)) sendDocumentThumbnail)
        $ (maybe id (\t -> ((Input "caption" t):)) sendDocumentCaption)
        $ (maybe id (\t -> ((Input "parse_mode" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentParseMode)
        $ (maybe id (\t -> ((Input "caption_entities" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentCaptionEntities)
        $ (maybe id (\t -> ((Input "disable_notification" (bool "false" "true" t)):)) sendDocumentDisableNotification)
        $ (maybe id (\t -> ((Input "disable_content_type_detection" (bool "false" "true" t)):)) sendDocumentDisableContentTypeDetection)
        $ (maybe id (\t -> ((Input "protect_content" (bool "false" "true" t)):)) sendDocumentProtectContent)
        $ (maybe id (\t -> ((Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentReplyToMessageId)
        $ (maybe id (\t -> ((Input "reply_parameters" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentReplyParameters)
        $ (maybe id (\t -> ((Input "reply_markup" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentReplyMarkup)
        [])
    files
      = (FileData "file" (T.pack $ takeFileName path) ct path)
      : maybe [] (\t -> [FileData "thumbnail" (T.pack $ takeFileName t) "image/jpeg" t]) sendDocumentThumbnail

    DocumentFile path ct = sendDocumentDocument


instance ToJSON   SendDocumentRequest where toJSON = gtoJSON

makeDefault ''SendDocumentRequest

-- | Generate send document structure.
toSendDocument :: SomeChatId -> DocumentFile -> SendDocumentRequest
toSendDocument = defSendDocument
