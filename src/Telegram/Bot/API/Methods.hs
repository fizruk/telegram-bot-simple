{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Methods where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Text
import Data.Bool
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)
import Servant.Multipart.API
import Servant.Multipart.Client
import System.FilePath

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Data.Maybe (catMaybes)
import Data.Functor ((<&>))

-- * Available methods

-- ** 'getMe'

type GetMe = "getMe" :> Get '[JSON] (Response User)

-- | A simple method for testing your bot's auth token.
-- Requires no parameters.
-- Returns basic information about the bot in form of a 'User' object.
getMe :: ClientM (Response User)
getMe = client (Proxy @GetMe)

-- ** 'deleteMessage'

-- | Notice that deleting by POST method was bugged, so we use GET
type DeleteMessage = "deleteMessage"
  :> RequiredQueryParam "chat_id" ChatId
  :> RequiredQueryParam "message_id" MessageId
  :> Get '[JSON] (Response Bool)

-- | Use this method to delete message in chat.
-- On success, the sent Bool is returned.
deleteMessage :: ChatId -> MessageId -> ClientM (Response Bool)
deleteMessage = client (Proxy @DeleteMessage)

-- ** 'sendMessage'

type SendMessage
  = "sendMessage" :> ReqBody '[JSON] SendMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
sendMessage :: SendMessageRequest -> ClientM (Response Message)
sendMessage = client (Proxy @SendMessage)

-- ** 'forwardMessage'
type ForwardMessage
  = "forwardMessage" :> ReqBody '[JSON] ForwardMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to forward messages of any kind.
-- On success, the sent 'Message' is returned.

forwardMessage :: ForwardMessageRequest -> ClientM (Response Message)
forwardMessage = client (Proxy @ForwardMessage)

-- | Additional interface options.
-- A JSON-serialized object for an inline keyboard, custom reply keyboard,
-- instructions to remove reply keyboard or to force a reply from the user.
data SomeReplyMarkup
  = SomeInlineKeyboardMarkup InlineKeyboardMarkup
  | SomeReplyKeyboardMarkup  ReplyKeyboardMarkup
  | SomeReplyKeyboardRemove  ReplyKeyboardRemove
  | SomeForceReply           ForceReply
  deriving (Generic)

instance ToJSON   SomeReplyMarkup where toJSON = genericSomeToJSON
instance FromJSON SomeReplyMarkup where parseJSON = genericSomeParseJSON

data ParseMode
  = Markdown
  | HTML
  | MarkdownV2
  deriving (Generic)

instance ToJSON   ParseMode
instance FromJSON ParseMode

-- | Request parameters for 'sendMessage'.
data SendMessageRequest = SendMessageRequest
  { sendMessageChatId                :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendMessageText                  :: Text -- ^ Text of the message to be sent.
  , sendMessageParseMode             :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendMessageDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , sendMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

instance ToJSON   SendMessageRequest where toJSON = gtoJSON
instance FromJSON SendMessageRequest where parseJSON = gparseJSON

-- | Request parameters for 'forwardMessage'.
data ForwardMessageRequest = ForwardMessageRequest
  { forwardMessageChatId              :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername).
  , forwardMessageFromChatId          :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @\@channelusername)
  , forwardMessageDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , forwardMessageMessageId           :: MessageId -- ^ Message identifier in the chat specified in from_chat_id
  } deriving (Generic)

instance ToJSON   ForwardMessageRequest where toJSON = gtoJSON
instance FromJSON ForwardMessageRequest where parseJSON = gparseJSON

-- ** 'sendMessage'
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
  { sendDocumentChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendDocumentDocument :: DocumentFile -- ^ Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data
  , sendDocumentThumb :: Maybe FilePath -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>
  , sendDocumentCaption :: Maybe Text -- ^ Document caption (may also be used when resending documents by file_id), 0-1024 characters after entities parsing
  , sendDocumentParseMode :: Maybe ParseMode -- ^ Mode for parsing entities in the document caption.
  , sendDocumentDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendDocumentReplyToMessageId :: Maybe MessageId
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
      (   (maybe id (\_ -> ((Input "thumb" "attach://thumb"):)) sendDocumentThumb)
        $ (maybe id (\t -> ((Input "caption" t):)) sendDocumentCaption)
        $ (maybe id (\t -> ((Input "parse_mode" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentParseMode)
        $ (maybe id (\t -> ((Input "disable_notification" (bool "false" "true" t)):)) sendDocumentDisableNotification)
        $ (maybe id (\t -> ((Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentReplyToMessageId)
        $ (maybe id (\t -> ((Input "reply_markup" (TL.toStrict $ encodeToLazyText t)):)) sendDocumentReplyMarkup)
        [])
    files
      = (FileData "file" (T.pack $ takeFileName path) ct path)
      : maybe [] (\t -> [FileData "thumb" (T.pack $ takeFileName t) "image/jpeg" t]) sendDocumentThumb

    DocumentFile path ct = sendDocumentDocument


instance ToJSON   SendDocumentRequest where toJSON = gtoJSON

-- | Generate send document structure.
toSendDocument :: SomeChatId -> DocumentFile -> SendDocumentRequest
toSendDocument ch df = SendDocumentRequest
  { sendDocumentChatId = ch
  , sendDocumentDocument = df
  , sendDocumentThumb = Nothing
  , sendDocumentCaption = Nothing
  , sendDocumentParseMode = Nothing
  , sendDocumentDisableNotification = Nothing
  , sendDocumentReplyToMessageId = Nothing
  , sendDocumentReplyMarkup = Nothing
  }

-- ** 'getFile'
type GetFile
  = "getFile"
  :> RequiredQueryParam "file_id" FileId
  :> Get '[JSON] (Response File)

getFile :: FileId -> ClientM (Response File)
getFile = client (Proxy @GetFile)

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
  { sendPhotoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendPhotoPhoto :: PhotoFile -- ^ Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data
  , sendPhotoThumb :: Maybe FilePath -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>
  , sendPhotoCaption :: Maybe Text -- ^ Photo caption (may also be used when resending Photos by file_id), 0-1024 characters after entities parsing
  , sendPhotoParseMode :: Maybe ParseMode -- ^ Mode for parsing entities in the Photo caption.
  , sendPhotoDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendPhotoReplyToMessageId :: Maybe MessageId
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
      (   (maybe id (\_ -> ((Input "thumb" "attach://thumb"):)) sendPhotoThumb)
        $ (maybe id (\t -> ((Input "caption" t):)) sendPhotoCaption)
        $ (maybe id (\t -> ((Input "parse_mode" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoParseMode)
        $ (maybe id (\t -> ((Input "disable_notification" (bool "false" "true" t)):)) sendPhotoDisableNotification)
        $ (maybe id (\t -> ((Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)):)) sendPhotoReplyToMessageId)
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

-- | Request parameters for 'copyMessage'.
data CopyMessageRequest = CopyMessageRequest
  { copyMessageChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , copyMessageFromChatId :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)
  , copyMessageMessageId :: MessageId -- ^ Message identifier in the chat specified in from_chat_id
  , copyMessageCaption :: Maybe Text -- ^ New caption for media, 0-1024 characters after entities parsing. If not specified, the original caption is kept
  , copyMessageParseMode :: Maybe Text -- ^ Mode for parsing entities in the new caption. See formatting options for more details.
  , copyMessageCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , copyMessageDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , copyMessageProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , copyMessageReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , copyMessageAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , copyMessageReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type CopyMessage
  = "copyMessage"
  :> ReqBody '[JSON] CopyMessageRequest
  :> Post '[JSON] (Response MessageId)

-- | Use this method to copy messages of any kind.
--   Service messages and invoice messages can't be
--   copied. The method is analogous to the method
--   forwardMessage, but the copied message doesn't
--   have a link to the original message.
--   Returns the MessageId of the sent message on success.
copyMessage :: CopyMessageRequest ->  ClientM (Response MessageId)
copyMessage = client (Proxy @CopyMessage)

-- | Request parameters for 'sendAudio'.
data SendAudioRequest = SendAudioRequest
  { sendAudioChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendAudioAudio :: InputFile -- ^ Audio to send. Pass a file_id as String to send an audio that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a audio from the Internet, or upload a new audio using multipart/form-data. More info on Sending Files »
  , sendAudioDuration :: Maybe Int -- ^ Duration of sent audio in seconds
  , sendAudioPerformer :: Maybe Text -- ^ Performer
  , sendAudioTitle :: Maybe Text -- ^ Track name
  , sendAudioThumb :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendAudioCaption :: Maybe Text -- ^ Audio caption (may also be used when resending audios by file_id), 0-1024 characters after entities parsing
  , sendAudioParseMode :: Maybe Text -- ^ Mode for parsing entities in the audio caption. See formatting options for more details.
  , sendAudioCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendAudioDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendAudioProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendAudioReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendAudioAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendAudioReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendAudioRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendAudioRequest where
  toMultipart SendAudioRequest{..} =
    maybe id (makeFile "thumb") sendAudioThumb $
    makeFile "audio" sendAudioAudio $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendAudioChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendAudioCaption <&>
        \t -> Input "caption" t
      , sendAudioParseMode <&>
        \t -> Input "parse_mode" t
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
        \t -> Input "protected_content" (bool "false" "true" t)
      , sendAudioReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendAudioAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
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
sendAudio r = case (sendAudioAudio r, sendAudioThumb r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAudioContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAudioContent) (boundary, r)
  _ ->  client (Proxy @SendAudioLink) r

-- | Request parameters for 'sendVideo'.
data SendVideoRequest = SendVideoRequest
  { sendVideoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendVideoVideo :: InputFile -- ^ Video to send. Pass a file_id as String to send an video that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a video from the Internet, or upload a new video using multipart/form-data. More info on Sending Files »
  , sendVideoDuration :: Maybe Int -- ^ Duration of sent video in seconds
  , sendVideoWidth :: Maybe Int -- ^ Video width
  , sendVideoHeight :: Maybe Int -- ^ Video height
  , sendVideoThumb :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendVideoCaption :: Maybe Text -- ^ Video caption (may also be used when resending videos by file_id), 0-1024 characters after entities parsing
  , sendVideoParseMode :: Maybe Text -- ^ Mode for parsing entities in the video caption. See formatting options for more details.
  , sendVideoCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendVideoSupportsStreaming :: Maybe Bool -- ^ Pass True, if the uploaded video is suitable for streaming
  , sendVideoDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVideoProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendVideoReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVideoAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVideoReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendVideoRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendVideoRequest where
  toMultipart SendVideoRequest{..} =
    maybe id (makeFile "thumb") sendVideoThumb $
    makeFile "video" sendVideoVideo $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendVideoChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendVideoCaption <&>
        \t -> Input "caption" t
      , sendVideoParseMode <&>
        \t -> Input "parse_mode" t
      , sendVideoCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
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
        \t -> Input "protected_content" (bool "false" "true" t)
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
sendVideo r = case (sendVideoVideo r, sendVideoThumb r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoContent) (boundary, r)
  _ ->  client (Proxy @SendVideoLink) r

-- | Request parameters for 'sendAnimation'.
data SendAnimationRequest = SendAnimationRequest
  { sendAnimationChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendAnimationAnimation :: InputFile -- ^ Animation to send. Pass a file_id as String to send an animation that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get an animation from the Internet, or upload a new animation using multipart/form-data. More info on Sending Files »
  , sendAnimationDuration :: Maybe Int -- ^ Duration of sent animation in seconds
  , sendAnimationWidth :: Maybe Int -- ^ Animation width
  , sendAnimationHeight :: Maybe Int -- ^ Animation height
  , sendAnimationThumb :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
  , sendAnimationCaption :: Maybe Text -- ^ Animation caption (may also be used when resending animation by file_id), 0-1024 characters after entities parsing
  , sendAnimationParseMode :: Maybe Text -- ^ Mode for parsing entities in the animation caption. See formatting options for more details.
  , sendAnimationCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , sendAnimationDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendAnimationProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendAnimationReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendAnimationAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendAnimationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendAnimationRequest where toJSON = gtoJSON

instance ToMultipart Tmp SendAnimationRequest where
  toMultipart SendAnimationRequest{..} =
    maybe id (makeFile "thumb") sendAnimationThumb $
    makeFile "animation" sendAnimationAnimation $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendAnimationChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendAnimationCaption <&>
        \t -> Input "caption" t
      , sendAnimationParseMode <&>
        \t -> Input "parse_mode" t
      , sendAnimationCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationWidth <&>
        \t -> Input "width" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationHeight <&>
        \t -> Input "height" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendAnimationProtectContent <&>
        \t -> Input "protected_content" (bool "false" "true" t)
      , sendAnimationReplyToMessageId <&>
        \t -> Input "reply_to_message_id" (TL.toStrict $ encodeToLazyText t)
      , sendAnimationAllowSendingWithoutReply <&>
        \t -> Input "allow_sending_without_reply" (bool "false" "true" t)
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
sendAnimation r = case (sendAnimationAnimation r, sendAnimationThumb r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAnimationContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendAnimationContent) (boundary, r)
  _ ->  client (Proxy @SendAnimationLink) r

-- | Request parameters for 'sendVoice'.
data SendVoiceRequest = SendVoiceRequest
  { sendVoiceChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendVoiceVoice :: InputFile -- ^ Audio file to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data. More info on Sending Files »
  , sendVoiceCaption :: Maybe Text -- ^ Voice message caption, 0-1024 characters after entities parsing
  , sendVoiceParseMode :: Maybe Text -- ^ Mode for parsing entities in the voice message caption. See formatting options for more details.
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
      [ sendVoiceCaption <&>
        \t -> Input "caption" t
      , sendVoiceParseMode <&>
        \t -> Input "parse_mode" t
      , sendVoiceCaptionEntities <&>
        \t -> Input "caption_entities" (TL.toStrict $ encodeToLazyText t)
      , sendVoiceDuration <&>
        \t -> Input "duration" (TL.toStrict $ encodeToLazyText t)
      , sendVoiceProtectContent <&>
        \t -> Input "protected_content" (bool "false" "true" t)
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

-- | Request parameters for 'sendVideoNote'.
data SendVideoNoteRequest = SendVideoNoteRequest
  { sendVideoNoteChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendVideoNoteVideoNote :: InputFile -- ^ Video note to send. Pass a file_id as String to send a video note that exists on the Telegram servers (recommended) or upload a new video using multipart/form-data. More info on Sending Files ». Sending video notes by a URL is currently unsupported
  , sendVideoNoteDuration :: Maybe Int -- ^ Duration of sent video in seconds
  , sendVideoNoteLength :: Maybe Int -- ^ Video width and height, i.e. diameter of the video message
  , sendVideoNoteThumb :: Maybe InputFile -- ^ Thumbnail of the file sent; can be ignored if thumbnail generation for the file is supported server-side. The thumbnail should be in JPEG format and less than 200 kB in size. A thumbnail's width and height should not exceed 320. Ignored if the file is not uploaded using multipart/form-data. Thumbnails can't be reused and can be only uploaded as a new file, so you can pass “attach://<file_attach_name>” if the thumbnail was uploaded using multipart/form-data under <file_attach_name>. More info on Sending Files »
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
    maybe id (makeFile "thumb") sendVideoNoteThumb $
    makeFile "video_note" sendVideoNoteVideoNote $
    MultipartData fields [] where
    fields =
      [ Input "chat_id" $ case sendVideoNoteChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ] <> catMaybes
      [ sendVideoNoteDisableNotification <&>
        \t -> Input "disable_notification" (bool "false" "true" t)
      , sendVideoNoteProtectContent <&>
        \t -> Input "protected_content" (bool "false" "true" t)
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
sendVideoNote r = case (sendVideoNoteVideoNote r, sendVideoNoteThumb r) of
  (InputFile{}, _) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoNoteContent) (boundary, r)
  (_, Just InputFile{}) -> do
    boundary <- liftIO genBoundary
    client (Proxy @SendVideoNoteContent) (boundary, r)
  _ ->  client (Proxy @SendVideoNoteLink) r

-- | Request parameters for 'sendMediaGroup'.
data SendMediaGroupRequest = SendMediaGroupRequest
  { sendMediaGroupChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendMediaGroupMedia :: [InputMedia] -- ^ A JSON-serialized array describing messages to be sent, must include 2-10 items. InputMediaAudio, InputMediaDocument, InputMediaPhoto or InputMediaVideo.
  , sendMediaGroupDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendMediaGroupProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendMediaGroupReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendMediaGroupAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendMediaGroupReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendMediaGroupRequest where toJSON = gtoJSON

type SendMediaGroup = "sendMediaGroup"
  :> ReqBody '[JSON] SendMediaGroupRequest
  :> Post '[JSON] (Response [Message])

-- | Use this method to send a group of photos, videos,
--   documents or audios as an album. Documents
--   and audio files can be only grouped in an album
--   with messages of the same type.
--   On success, an array of Messages that were sent is returned.
sendMediaGroup :: SendMediaGroupRequest ->  ClientM (Response [Message])
sendMediaGroup = client (Proxy @SendMediaGroup)

-- | Request parameters for 'sendLocation'.
data SendLocationRequest = SendLocationRequest
  { sendLocationChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendLocationLatitude :: Float -- ^ Latitude of new location
  , sendLocationLongitude :: Float -- ^ Longitude of new location
  , sendLocationHorizontalAccuracy :: Maybe Float -- ^ The radius of uncertainty for the location, measured in meters; 0-1500
  , sendLocationLivePeriod :: Int -- ^ Period in seconds for which the location will be updated (see Live Locations, should be between 60 and 86400.)
  , sendLocationHeading :: Maybe Int -- ^ Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  , sendLocationProximityAlertRadius :: Maybe Int  -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  , sendLocationDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendLocationProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendLocationReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendLocationAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type SendLocation = "sendLocation"
  :> ReqBody '[JSON] SendLocationRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send point on the map.
--   On success, the sent Message is returned.
sendLocation :: SendLocationRequest ->  ClientM (Response Message)
sendLocation = client (Proxy @SendLocation)

-- | Request parameters for 'editMessageLiveLocation'.
data EditMessageLiveLocationRequest = EditMessageLiveLocationRequest
  { editMessageLiveLocationChatId :: Maybe SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editMessageLiveLocationMessageId :: Maybe MessageId -- ^ Required if inline_message_id is not specified. Identifier of the message with live location to stop
  , editMessageLiveLocationInlineMessageId :: Maybe Text -- ^  	Required if chat_id and message_id are not specified. Identifier of the inline message
  , editMessageLiveLocationLatitude :: Float -- ^ Latitude of new location
  , editMessageLiveLocationLongitude :: Float -- ^ Longitude of new location
  , editMessageLiveLocationHorizontalAccuracy :: Maybe Float -- ^ The radius of uncertainty for the location, measured in meters; 0-1500
  , editMessageLiveLocationHeading :: Maybe Int -- ^ Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  , editMessageLiveLocationProximityAlertRadius :: Maybe Int  -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  , editMessageLiveLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type EditMessageLiveLocation = "editMessageLiveLocation"
  :> ReqBody '[JSON] EditMessageLiveLocationRequest
  :> Post '[JSON] (Response Message)

-- FIXME: Add Bool returning in case of inline message. 

-- | Use this method to edit live location messages.
--   A location can be edited until its live_period
--   expires or editing is explicitly disabled by a
--   call to stopMessageLiveLocation. On success, if
--   the edited message is not an inline message, the
--   edited Message is returned, otherwise True is returned.
editMessageLiveLocation :: EditMessageLiveLocationRequest ->  ClientM (Response Message)
editMessageLiveLocation = client (Proxy @EditMessageLiveLocation)

-- | Request parameters for 'stopMessageLiveLocation'.
data StopMessageLiveLocationRequest = StopMessageLiveLocationRequest
  { stopMessageLiveLocationChatId :: Maybe SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , stopMessageLiveLocationMessageId :: Maybe MessageId -- ^ Required if inline_message_id is not specified. Identifier of the message with live location to stop
  , stopMessageLiveLocationInlineMessageId :: Maybe Text -- ^  	Required if chat_id and message_id are not specified. Identifier of the inline message
  , stopMessageLiveLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type StopMessageLiveLocation = "stopMessageLiveLocation"
  :> ReqBody '[JSON] StopMessageLiveLocationRequest
  :> Post '[JSON] (Response Message)

-- FIXME: Add Bool returning in case of inline message. 

-- | Use this method to stop updating a live
--   location message before live_period
--   expires. On success, if the message is
--   not an inline message, the edited Message
--   is returned, otherwise True is returned.
stopMessageLiveLocation :: StopMessageLiveLocationRequest ->  ClientM (Response Message)
stopMessageLiveLocation = client (Proxy @StopMessageLiveLocation)

-- | Request parameters for 'sendVenue'.
data SendVenueRequest = SendVenueRequest
  { sendVenueChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendVenueLatitude :: Float -- ^ Latitude of the venue
  , sendVenueLongitude :: Float -- ^ Longitude of the venue
  , sendVenueTitle :: Text -- ^ Name of the venue
  , sendVenueAddress :: Text -- ^ Address of the venue
  , sendVenueFoursquareId :: Maybe Text -- ^ Foursquare identifier of the venue
  , sendVenueFoursquareType :: Maybe Text -- ^ Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  , sendVenueGooglePlaceId :: Maybe Text -- ^ Google Places identifier of the venue
  , sendVenueGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See supported types <https:\/\/developers.google.com\/maps\/documentation\/places\/web-service\/supported_types>.)
  , sendVenueDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVenueProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendVenueReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVenueAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVenueReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type SendVenue = "sendVenue"
  :> ReqBody '[JSON] SendVenueRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send information about a venue.
--   On success, the sent Message is returned.
sendVenue :: SendVenueRequest ->  ClientM (Response Message)
sendVenue = client (Proxy @SendVenue)

-- | Request parameters for 'sendContact'.
data SendContactRequest = SendContactRequest
  { sendContactChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendContactPhoneNumber :: Text -- ^ Contact's phone number
  , sendContactFirstName  :: Text -- ^ Contact's first name
  , sendContactLastName  :: Text -- ^ Contact's last name
  , sendContactVcard  :: Text -- ^ Additional data about the contact in the form of a vCard, 0-2048 bytes
  , sendContactDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendContactProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendContactReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendContactAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendContactReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type SendContact = "sendContact"
  :> ReqBody '[JSON] SendContactRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send phone contacts.
--   On success, the sent Message is returned.
sendContact :: SendContactRequest ->  ClientM (Response Message)
sendContact = client (Proxy @SendContact)

-- | Request parameters for 'sendPoll'.
data SendPollRequest = SendPollRequest
  { sendPollChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendPollQuestion :: Text -- ^ Poll question, 1-300 characters
  , sendPollOptions :: [Text] -- ^ A JSON-serialized list of answer options, 2-10 strings 1-100 characters each
  , sendPollIsAnonymous :: Maybe Bool -- ^ True, if the poll needs to be anonymous, defaults to True
  , sendPollType :: Maybe Text -- ^ Poll type, “quiz” or “regular”, defaults to “regular”
  , sendPollAllowsMultipleAnswers :: Maybe Bool -- ^ True, if the poll allows multiple answers, ignored for polls in quiz mode, defaults to False
  , sendPollCorrectOptionId :: Maybe Int -- ^ 0-based identifier of the correct answer option, required for polls in quiz mode
  , sendPollExplanation :: Maybe Text -- ^ Text that is shown when a user chooses an incorrect answer or taps on the lamp icon in a quiz-style poll, 0-200 characters with at most 2 line feeds after entities parsing
  , sendPollExplanationParseMode :: Maybe Text -- ^ Mode for parsing entities in the explanation. See formatting options for more details.
  , sendPollExplanationEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the poll explanation, which can be specified instead of parse_mode
  , sendPollOpenPeriod :: Maybe Int -- ^ Amount of time in seconds the poll will be active after creation, 5-600. Can't be used together with close_date.
  , sendPollCloseDate :: Maybe Int -- ^ Point in time (Unix timestamp) when the poll will be automatically closed. Must be at least 5 and no more than 600 seconds in the future. Can't be used together with open_period.
  , sendPollIsClosed :: Maybe Bool -- ^ Pass True, if the poll needs to be immediately closed. This can be useful for poll preview.
  , sendPollDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendPollProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendPollReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendPollAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendPollReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type SendPoll = "sendPoll"
  :> ReqBody '[JSON] SendPollRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send a native poll.
--   On success, the sent Message is returned.
sendPoll :: SendPollRequest ->  ClientM (Response Message)
sendPoll = client (Proxy @SendPoll)

-- | Request parameters for 'sendDice'.
data SendDiceRequest = SendDiceRequest
  { sendDiceChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , sendDiceEmoji :: Maybe Text -- ^ Emoji on which the dice throw animation is based. Currently, must be one of “🎲”, “🎯”, “🏀”, “⚽”, “🎳”, or “🎰”. Dice can have values 1-6 for “🎲”, “🎯” and “🎳”, values 1-5 for “🏀” and “⚽”, and values 1-64 for “🎰”. Defaults to “🎲”
  , sendDiceDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendDiceProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding
  , sendDiceReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendDiceAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendDiceReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

type SendDice = "sendDice"
  :> ReqBody '[JSON] SendDiceRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send an animated emoji that
--   will display a random value.
--   On success, the sent Message is returned.
sendDice :: SendDiceRequest ->  ClientM (Response Message)
sendDice = client (Proxy @SendDice)

type SendChatAction = "sendChatAction"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "action" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method when you need to tell the
--   user that something is happening on the bot's side.
--   The status is set for 5 seconds or less
--   (when a message arrives from your bot, Telegram
--   clients clear its typing status).
--   Returns True on success.
--
--   Example: The ImageBot needs some time to
--   process a request and upload the image.
--   Instead of sending a text message along
--   the lines of “Retrieving image, please wait…”,
--   the bot may use sendChatAction with action = upload_photo.
--   The user will see a “sending photo” status for the bot.
--
--   We only recommend using this method when a
--   response from the bot will take a noticeable
--   amount of time to arrive.
sendChatAction :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ Type of action to broadcast. Choose one, depending on what the user is about to receive: typing for text messages, upload_photo for photos, record_video or upload_video for videos, record_voice or upload_voice for voice notes, upload_document for general files, choose_sticker for stickers, find_location for location data, record_video_note or upload_video_note for video notes.
  -> ClientM (Response  Bool)
sendChatAction = client (Proxy @SendChatAction)

-- | Request parameters for 'getUserProfilePhotos'.
data GetUserProfilePhotosRequest = GetUserProfilePhotosRequest
  { getUserProfilePhotosUserId :: UserId -- ^ Unique identifier of the target user
  , getUserProfilePhotosOffset :: Maybe Int -- ^ Sequential number of the first photo to be returned. By default, all photos are returned.
  , getUserProfilePhotosLimit :: Maybe Int -- ^ Limits the number of photos to be retrieved. Values between 1-100 are accepted. Defaults to 100.
  }
  deriving Generic

type GetUserProfilePhotos = "getUserProfilePhotos"
  :> ReqBody '[JSON] GetUserProfilePhotosRequest
  :> Post '[JSON] (Response UserProfilePhotos)

-- | Use this method to get a list of profile pictures for a user.
--   Returns a UserProfilePhotos object.
getUserProfilePhotos :: GetUserProfilePhotosRequest ->  ClientM (Response UserProfilePhotos)
getUserProfilePhotos = client (Proxy @GetUserProfilePhotos)

-- | Request parameters for 'banChatMember'.
data BanChatMemberRequest = BanChatMemberRequest
  { banChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , banChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , banChatMemberUntilDate :: Maybe Int -- ^ Date when the user will be unbanned, unix time. If user is banned for more than 366 days or less than 30 seconds from the current time they are considered to be banned forever. Applied for supergroups and channels only.
  , banChatMemberRevokeMessages :: Maybe Bool -- ^ Pass True to delete all messages from the chat for the user that is being removed. If False, the user will be able to see messages in the group that were sent before the user was removed. Always True for supergroups and channels.
  }
  deriving Generic

type BanChatMember = "banChatMember"
  :> ReqBody '[JSON] BanChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to ban a user in a
--   group, a supergroup or a channel.
--   In the case of supergroups and channels,
--   the user will not be able to return to
--   the chat on their own using invite links,
--   etc., unless unbanned first. The bot must
--   be an administrator in the chat for this
--   to work and must have the appropriate
--   administrator rights.
--   Returns True on success.
banChatMember :: BanChatMemberRequest ->  ClientM (Response Bool)
banChatMember = client (Proxy @BanChatMember)

-- | Request parameters for 'unbanChatMember'.
data UnbanChatMemberRequest = UnbanChatMemberRequest
  { unbanChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , unbanChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , unbanChatMemberOnlyIfBanned :: Maybe Bool -- ^ Do nothing if the user is not banned
  }
  deriving Generic

type UnbanChatMember = "unbanChatMember"
  :> ReqBody '[JSON] UnbanChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to unban a previously
--   banned user in a supergroup or channel.
--   The user will not return to the group
--   or channel automatically, but will be
--   able to join via link, etc. The bot must
--   be an administrator for this to work. By
--   default, this method guarantees that after
--   the call the user is not a member of the chat,
--   but will be able to join it. So if the user is
--   a member of the chat they will also be removed
--   from the chat. If you don't want this, use the
--   parameter only_if_banned.
--   Returns True on success.
unbanChatMember :: UnbanChatMemberRequest ->  ClientM (Response Bool)
unbanChatMember = client (Proxy @UnbanChatMember)

-- | Request parameters for 'restrictChatMember'.
data RestrictChatMemberRequest = RestrictChatMemberRequest
  { restrictChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , restrictChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , restrictChatMemberPermissions :: ChatPermissions -- ^ A JSON-serialized object for new user permissions
  , restrictChatMemberUntilDate :: Maybe Int -- ^ Date when restrictions will be lifted for the user, unix time. If user is restricted for more than 366 days or less than 30 seconds from the current time, they are considered to be restricted forever
  }
  deriving Generic

type RestrictChatMember = "restrictChatMember"
  :> ReqBody '[JSON] RestrictChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to restrict a user
--   in a supergroup. The bot must be an
--   administrator in the supergroup for
--   this to work and must have the appropriate
--   administrator rights. Pass True for all
--   permissions to lift restrictions from a
--   user.
--   Returns True on success.
restrictChatMember :: RestrictChatMemberRequest ->  ClientM (Response Bool)
restrictChatMember = client (Proxy @RestrictChatMember)

-- | Request parameters for 'promoteChatMember'.
data PromoteChatMemberRequest = PromoteChatMemberRequest
  { promoteChatMemberChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , promoteChatMemberUserId :: UserId -- ^ Unique identifier of the target user
  , promoteChatMemberIsAnonymous :: Maybe Bool -- ^ Pass True, if the administrator's presence in the chat is hidden
  , promoteChatMemberCanManageChat :: Maybe Bool -- ^ Pass True, if the administrator can access the chat event log, chat statistics, message statistics in channels, see channel members, see anonymous administrators in supergroups and ignore slow mode. Implied by any other administrator privilege
  , promoteChatMemberCanPostMessages :: Maybe Bool -- ^ Pass True, if the administrator can create channel posts, channels only
  , promoteChatMemberCanEditMessages :: Maybe Bool -- ^ Pass True, if the administrator can edit messages of other users and can pin messages, channels only
  , promoteChatMemberCanDeleteMessages :: Maybe Bool -- ^ Pass True, if the administrator can delete messages of other users
  , promoteChatMemberCanManageVoiceChats :: Maybe Bool -- ^ Pass True, if the administrator can manage voice chats
  , promoteChatMemberCanRestrictMembers :: Maybe Bool -- ^ Pass True, if the administrator can restrict, ban or unban chat members
  , promoteChatMemberCanPromoteMembers :: Maybe Bool -- ^ Pass True, if the administrator can add new administrators with a subset of their own privileges or demote administrators that he has promoted, directly or indirectly (promoted by administrators that were appointed by him)
  , promoteChatMemberCanChangeInfo :: Maybe Bool -- ^ Pass True, if the administrator can change chat title, photo and other settings
  , promoteChatMemberCanInviteUsers :: Maybe Bool -- ^ Pass True, if the administrator can invite new users to the chat
  , promoteChatMemberCanPinMessages :: Maybe Bool -- ^ Pass True, if the administrator can pin messages, supergroups only
  }
  deriving Generic

type PromoteChatMember = "promoteChatMember"
  :> ReqBody '[JSON] PromoteChatMemberRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to promote or demote
--   a user in a supergroup or a channel.
--   The bot must be an administrator in
--   the chat for this to work and must have
--   the appropriate administrator rights.
--   Pass False for all boolean parameters
--   to demote a user.
--   Returns True on success.
promoteChatMember ::PromoteChatMemberRequest ->  ClientM (Response Bool)
promoteChatMember = client (Proxy @PromoteChatMember)

-- | Request parameters for 'setChatAdministratorCustomTitle'.
data SetChatAdministratorCustomTitleRequest = SetChatAdministratorCustomTitleRequest
  { setChatAdministratorCustomTitleChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , setChatAdministratorCustomTitleUserId :: UserId -- ^ Unique identifier of the target user
  , setChatAdministratorCustomTitleCustomTitle :: Text -- ^ New custom title for the administrator; 0-16 characters, emoji are not allowed
  }
  deriving Generic

type SetChatAdministratorCustomTitle = "setChatAdministratorCustomTitle"
  :> ReqBody '[JSON] SetChatAdministratorCustomTitleRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a custom title
--   for an administrator in a supergroup
--   promoted by the bot.
--   Returns True on success.
setChatAdministratorCustomTitle :: SetChatAdministratorCustomTitleRequest ->  ClientM (Response Bool)
setChatAdministratorCustomTitle = client (Proxy @SetChatAdministratorCustomTitle)

type BanChatSenderChat = "banChatSenderChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sender_chat_id" ChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to ban a channel chat
--   in a supergroup or a channel. Until the
--   chat is unbanned, the owner of the banned
--   chat won't be able to send messages on
--   behalf of any of their channels. The bot
--   must be an administrator in the supergroup
--   or channel for this to work and must have
--   the appropriate administrator rights.
--   Returns True on success.
banChatSenderChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ChatId -- ^ Unique identifier of the target sender chat
  -> ClientM (Response  Bool)
banChatSenderChat = client (Proxy @BanChatSenderChat)

type UnbanChatSenderChat = "unbanChatSenderChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sender_chat_id" ChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to unban a previously
--   banned channel chat in a supergroup
--   or channel. The bot must be an administrator
--   for this to work and must have the appropriate
--   administrator rights.
--   Returns True on success.
unbanChatSenderChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ChatId -- ^ Unique identifier of the target sender chat
  -> ClientM (Response  Bool)
unbanChatSenderChat = client (Proxy @UnbanChatSenderChat)

-- | Request parameters for 'setChatPermissions'.
data SetChatPermissionsRequest = SetChatPermissionsRequest
  { setChatPermissionsChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , setChatPermissionsPermissions :: ChatPermissions -- ^ A JSON-serialized object for new default chat permissions
  }
  deriving Generic

type SetChatPermissions = "setChatPermissions"
  :> ReqBody '[JSON] SetChatPermissionsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set default chat
--   permissions for all members. The bot
--   must be an administrator in the group
--   or a supergroup for this to work and must
--   have the can_restrict_members administrator rights.
--   Returns True on success.
setChatPermissions :: SetChatPermissionsRequest ->  ClientM (Response Bool)
setChatPermissions = client (Proxy @SetChatPermissions)

type ExportChatInviteLink = "exportChatInviteLink"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Text)

-- | Use this method to generate a new
--   primary invite link for a chat; any
--   previously generated primary link is
--   revoked. The bot must be an administrator
--   in the chat for this to work and must have
--   the appropriate administrator rights.
--   Returns the new invite link as String on success.
exportChatInviteLink :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response  Text)
exportChatInviteLink = client (Proxy @ExportChatInviteLink)

-- | Request parameters for 'createChatInviteLink'.
data CreateChatInviteLinkRequest = CreateChatInviteLinkRequest
  { createChatInviteLinkChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , createChatInviteLinkName :: Maybe Text -- ^ Invite link name; 0-32 characters
  , createChatInviteLinkExpireDate :: Maybe Integer -- ^ Point in time (Unix timestamp) when the link will expire
  , createChatInviteLinkMemberLimit :: Maybe Int -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  , createChatInviteLinkCreatesJoinRequest :: Maybe Bool -- ^ True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
  }
  deriving Generic

type CreateChatInviteLink = "createChatInviteLink"
  :> ReqBody '[JSON] CreateChatInviteLinkRequest
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to create an additional
--   invite link for a chat. The bot must be 
--   an administrator in the chat for this to 
--   work and must have the appropriate administrator 
--   rights. The link can be revoked using the 
--   method revokeChatInviteLink. 
--   Returns the new invite link as ChatInviteLink object.
createChatInviteLink :: CreateChatInviteLinkRequest ->  ClientM (Response ChatInviteLink)
createChatInviteLink = client (Proxy @CreateChatInviteLink)

-- | Request parameters for 'editChatInviteLink'.
data EditChatInviteLinkRequest = EditChatInviteLinkRequest
  { editChatInviteLinkChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editChatInviteLinkInviteLink :: Text -- ^	The invite link to edit
  , editChatInviteLinkName :: Maybe Text -- ^ Invite link name; 0-32 characters
  , editChatInviteLinkExpireDate :: Maybe Integer -- ^ Point in time (Unix timestamp) when the link will expire
  , editChatInviteLinkMemberLimit :: Maybe Int -- ^ Maximum number of users that can be members of the chat simultaneously after joining the chat via this invite link; 1-99999
  , editChatInviteLinkCreatesJoinRequest :: Maybe Bool -- ^ True, if users joining the chat via the link need to be approved by chat administrators. If True, member_limit can't be specified
  }
  deriving Generic

type EditChatInviteLink = "editChatInviteLink"
  :> ReqBody '[JSON] EditChatInviteLinkRequest
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to edit a non-primary
--   invite link created by the bot. The 
--   bot must be an administrator in the 
--   chat for this to work and must have 
--   the appropriate administrator rights.
--   Returns the edited invite link as a ChatInviteLink object.
editChatInviteLink :: EditChatInviteLinkRequest ->  ClientM (Response ChatInviteLink)
editChatInviteLink = client (Proxy @EditChatInviteLink)

type RevokeChatInviteLink = "revokeChatInviteLink"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "invite_link" Text
  :> Post '[JSON] (Response ChatInviteLink)

-- | Use this method to revoke an invite
--   link created by the bot. If the primary 
--   link is revoked, a new link is automatically 
--   generated. The bot must be an administrator 
--   in the chat for this to work and must have 
--   the appropriate administrator rights. 
--   Returns the revoked invite link as ChatInviteLink object.
revokeChatInviteLink :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ The invite link to revoke
  -> ClientM (Response  ChatInviteLink)
revokeChatInviteLink = client (Proxy @RevokeChatInviteLink)

type ApproveChatJoinRequest = "approveChatJoinRequest"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response Bool)

-- | Use this method to approve a chat 
--   join request. The bot must be an 
--   administrator in the chat for this 
--   to work and must have the can_invite_users 
--   administrator right. 
--   Returns True on success.
approveChatJoinRequest :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ Unique identifier of the target user
  -> ClientM (Response Bool)
approveChatJoinRequest = client (Proxy @ApproveChatJoinRequest)

type DeclineChatJoinRequest = "declineChatJoinRequest"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response Bool)

-- | Use this method to decline a chat 
--   join request. The bot must be an 
--   administrator in the chat for this 
--   to work and must have the can_invite_users 
--   administrator right. 
--   Returns True on success.
declineChatJoinRequest :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ Unique identifier of the target user
  -> ClientM (Response Bool)
declineChatJoinRequest = client (Proxy @DeclineChatJoinRequest)

-- FIXME: Unsafe usage of InputFile - valid only InputFile constructor.

-- | Request parameters for 'setChatPhoto'.
data SetChatPhotoRequest = SetChatPhotoRequest
  { setChatPhotoChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , setChatPhotoPhoto :: InputFile -- ^ 	New chat photo, uploaded using multipart/form-data
  }

instance ToMultipart Tmp SetChatPhotoRequest where
  toMultipart SetChatPhotoRequest{..} =
    makeFile "photo" setChatPhotoPhoto (MultipartData fields []) where
    fields =
      [ Input "chat_id" $ case setChatPhotoChatId of
          SomeChatId (ChatId chat_id) -> T.pack $ show chat_id
          SomeChatUsername txt -> txt
      ]

type SetChatPhoto = "setChatPhoto"
  :> MultipartForm Tmp SetChatPhotoRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a new profile 
--   photo for the chat. Photos can't be changed
--   for private chats. The bot must be an 
--   administrator in the chat for this to work 
--   and must have the appropriate administrator rights. 
--   Returns True on success.
setChatPhoto :: SetChatPhotoRequest ->  ClientM (Response Bool)
setChatPhoto r =do
      boundary <- liftIO genBoundary
      client (Proxy @SetChatPhoto) (boundary, r)

type DeleteChatPhoto = "deleteChatPhoto"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a chat photo.
--   Photos can't be changed for private chats.
--   The bot must be an administrator in the chat 
--   for this to work and must have the appropriate 
--   administrator rights. 
--   Returns True on success.
deleteChatPhoto :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
deleteChatPhoto = client (Proxy @DeleteChatPhoto)

type SetChatTitle = "setChatTitle"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "title" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the title of
--   a chat. Titles can't be changed for private
--   chats. The bot must be an administrator in 
--   the chat for this to work and must have the 
--   appropriate administrator rights. 
--   Returns True on success.
setChatTitle :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ New chat title, 0-255 characters
  -> ClientM (Response Bool)
setChatTitle = client (Proxy @SetChatTitle)

type SetChatDescription = "setChatDescription"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> QueryParam "description" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the description 
--   of a group, a supergroup or a channel. The 
--   bot must be an administrator in the chat 
--   for this to work and must have the appropriate 
--   administrator rights. 
--   Returns True on success.
setChatDescription :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Maybe Text -- ^ New chat description, 0-255 characters
  -> ClientM (Response Bool)
setChatDescription = client (Proxy @SetChatDescription)

-- | Request parameters for 'pinChatMessage'.
data PinChatMessageRequest = PinChatMessageRequest
  { pinChatMessageChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , pinChatMessageMessageId :: MessageId -- ^ Identifier of a message to pin
  , pinChatMessageDisableNotification :: Maybe Bool -- ^ Pass True, if it is not necessary to send a notification to all chat members about the new pinned message. Notifications are always disabled in channels and private chats.
  }
  deriving Generic

type PinChatMessage = "pinChatMessage"
  :> ReqBody '[JSON] PinChatMessageRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to add a message to the list 
--   of pinned messages in a chat. If the chat is 
--   not a private chat, the bot must be an administrator 
--   in the chat for this to work and must have the 
--   'can_pin_messages' administrator right in a supergroup
--   or 'can_edit_messages' administrator right in a channel. 
--   Returns True on success.
pinChatMessage :: PinChatMessageRequest ->  ClientM (Response Bool)
pinChatMessage = client (Proxy @PinChatMessage)

type UnpinChatMessage = "unpinChatMessage"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> QueryParam "message_id" MessageId
  :> Post '[JSON] (Response Bool)

-- | Use this method to remove a message from the
--   list of pinned messages in a chat. If the chat 
--   is not a private chat, the bot must be an administrator
--   in the chat for this to work and must have the 
--   'can_pin_messages' administrator right in a supergroup 
--   or 'can_edit_messages' administrator right in a 
--   channel. 
--   Returns True on success.
unpinChatMessage :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Maybe MessageId -- ^ Identifier of a message to unpin. If not specified, the most recent pinned message (by sending date) will be unpinned.
  -> ClientM (Response Bool)
unpinChatMessage = client (Proxy @UnpinChatMessage)

type UnpinAllChatMessages = "unpinAllChatMessages"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to clear the list of pinned 
--   messages in a chat. If the chat is not a private 
--   chat, the bot must be an administrator in the 
--   chat for this to work and must have the 'can_pin_messages' 
--   administrator right in a supergroup or 'can_edit_messages' 
--   administrator right in a channel. 
--   Returns True on success.
unpinAllChatMessages :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
unpinAllChatMessages = client (Proxy @UnpinAllChatMessages)

type LeaveChat = "leaveChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method for your bot to leave a group, supergroup or channel. 
--   Returns True on success.
leaveChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
leaveChat = client (Proxy @LeaveChat)

type GetChat = "getChat"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Chat)

-- | Use this method to get up to date information 
--   about the chat (current name of the user for 
--   one-on-one conversations, current username of 
--   a user, group or channel, etc.). 
--   Returns a Chat object on success.
getChat :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Chat)
getChat = client (Proxy @GetChat)

type GetChatAdministrators = "getChatAdministrators"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response [ChatMember])

-- | Use this method to get a list of administrators
--   in a chat. On success, returns an Array of 
--   ChatMember objects that contains information 
--   about all chat administrators except other bots. 
--   If the chat is a group or a supergroup and no 
--   administrators were appointed, only the creator 
--   will be returned.
getChatAdministrators :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response [ChatMember])
getChatAdministrators = client (Proxy @GetChatAdministrators)

type GetChatMemberCount = "getChatMemberCount"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Integer)

-- | Use this method to get the number of members in a chat. 
--   Returns Int on success.
getChatMemberCount :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Integer)
getChatMemberCount = client (Proxy @GetChatMemberCount)

type GetChatMember = "getChatMember"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "user_id" UserId
  :> Post '[JSON] (Response ChatMember)

-- | Use this method to get information about a member of a chat. 
--   Returns a ChatMember object on success.
getChatMember :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> UserId -- ^ 	Unique identifier of the target user
  -> ClientM (Response ChatMember)
getChatMember = client (Proxy @GetChatMember)

type SetChatStickerSet = "setChatStickerSet"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> RequiredQueryParam "sticker_set_name" Text
  :> Post '[JSON] (Response Bool)

-- | Use this method to set a new group sticker
--   set for a supergroup. The bot must be an 
--   administrator in the chat for this to work 
--   and must have the appropriate administrator 
--   rights. Use the field can_set_sticker_set 
--   optionally returned in getChat requests to 
--   check if the bot can use this method. 
--   Returns True on success.
setChatStickerSet :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> Text -- ^ 	Name of the sticker set to be set as the group sticker set
  -> ClientM (Response Bool)
setChatStickerSet = client (Proxy @SetChatStickerSet)

type DeleteChatStickerSet = "deleteChatStickerSet"
  :> RequiredQueryParam "chat_id" SomeChatId
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete a group sticker 
--   set from a supergroup. The bot must be an 
--   administrator in the chat for this to work 
--   and must have the appropriate administrator 
--   rights. Use the field can_set_sticker_set 
--   optionally returned in getChat requests 
--   to check if the bot can use this method. 
--   Returns True on success.
deleteChatStickerSet :: SomeChatId -- ^ Unique identifier for the target chat or username of the target supergroup (in the format @supergroupusername)
  -> ClientM (Response Bool)
deleteChatStickerSet = client (Proxy @DeleteChatStickerSet)

-- | Request parameters for 'answerCallbackQuery'.
data AnswerCallbackQueryRequest = AnswerCallbackQueryRequest
  { answerCallbackQueryCallbackQueryId :: CallbackQueryId -- ^ Unique identifier for the query to be answered
  , answerCallbackQueryText :: Maybe Text -- ^ Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters
  , answerCallbackQueryShowAlert :: Maybe Bool -- ^ If True, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  , answerCallbackQueryUrl :: Maybe Text
    -- ^ URL that will be opened by the user's client. If you have created a Game and accepted the conditions via @Botfather, specify the URL that opens your game — note that this will only work if the query comes from a callback_game button.
    --
    --   Otherwise, you may use links like t.me/your_bot?start=XXXX that open your bot with a parameter.
  , answerCallbackQueryCacheTime :: Maybe Integer -- ^ The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0.
  }
  deriving Generic

type AnswerCallbackQuery = "answerCallbackQuery"
  :> ReqBody '[JSON] AnswerCallbackQueryRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to send answers to callback 
--   queries sent from inline keyboards. The answer 
--   will be displayed to the user as a notification 
--   at the top of the chat screen or as an alert. 
--   On success, True is returned.
--
--  Alternatively, the user can be redirected to 
--  the specified Game URL. For this option to work, 
--  you must first create a game for your bot via 
--  @Botfather and accept the terms. Otherwise, you 
--  may use links like t.me/your_bot?start=XXXX that 
--  open your bot with a parameter.
answerCallbackQuery :: AnswerCallbackQueryRequest ->  ClientM (Response Bool)
answerCallbackQuery = client (Proxy @AnswerCallbackQuery)

-- | Request parameters for 'setMyCommands'.
data SetMyCommandsRequest = SetMyCommandsRequest
  { setMyCommandsCommands :: [BotCommand] -- ^ A JSON-serialized list of bot commands to be set as the list of the bot's commands. At most 100 commands can be specified.
  , setMyCommandsScope :: Maybe BotCommandScope -- ^ A JSON-serialized object, describing scope of users for which the commands are relevant. Defaults to BotCommandScopeDefault.
  , setMyCommandsLanguageCode :: Maybe Text -- ^ A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
  }
  deriving Generic

type SetMyCommands = "setMyCommands"
  :> ReqBody '[JSON] SetMyCommandsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the list of 
--   the bot's commands. See <https:\/\/core.telegram.org\/bots#commands> 
--   for more details about bot commands. 
--   Returns True on success.
setMyCommands :: SetMyCommandsRequest ->  ClientM (Response Bool)
setMyCommands = client (Proxy @SetMyCommands)

-- | Request parameters for 'deleteMyCommands'.
data DeleteMyCommandsRequest = DeleteMyCommandsRequest
  { deleteMyCommandsScope :: Maybe BotCommandScope  -- ^ A JSON-serialized object, describing scope of users. Defaults to BotCommandScopeDefault. 
  , deleteMyCommandsLanguageCode :: Maybe Text  -- ^ 	A two-letter ISO 639-1 language code. If empty, commands will be applied to all users from the given scope, for whose language there are no dedicated commands
  }
  deriving Generic

type DeleteMyCommands = "deleteMyCommands"
  :> ReqBody '[JSON] DeleteMyCommandsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to delete the list of 
--   the bot's commands for the given scope 
--   and user language. After deletion, higher 
--   level commands will be shown to affected users. 
--   Returns True on success.
deleteMyCommands :: DeleteMyCommandsRequest -> ClientM (Response Bool)
deleteMyCommands = client (Proxy @DeleteMyCommands)

-- | Request parameters for 'getMyCommands'.
data GetMyCommandsRequest = GetMyCommandsRequest
  { getMyCommandsScope :: Maybe BotCommandScope  -- ^ A JSON-serialized object, describing scope of users. Defaults to BotCommandScopeDefault. 
  , getMyCommandsLanguageCode :: Maybe Text   -- ^ 	A two-letter ISO 639-1 language code or an empty string
  }
  deriving Generic

type GetMyCommands = "getMyCommands"
  :> ReqBody '[JSON] GetMyCommandsRequest
  :> Post '[JSON] (Response [BotCommand])

-- | Use this method to get the current list
--   of the bot's commands for the given scope 
--   and user language. Returns Array of BotCommand 
--   on success. If commands aren't set, an empty list 
--   is returned.
getMyCommands :: GetMyCommandsRequest -> ClientM (Response [BotCommand])
getMyCommands = client (Proxy @GetMyCommands)

foldMap deriveJSON'
  [ ''GetMyCommandsRequest
  , ''DeleteMyCommandsRequest
  , ''SetMyCommandsRequest
  , ''AnswerCallbackQueryRequest
  , ''EditChatInviteLinkRequest
  , ''PinChatMessageRequest
  , ''CreateChatInviteLinkRequest
  , ''SetChatPermissionsRequest
  , ''SetChatAdministratorCustomTitleRequest
  , ''PromoteChatMemberRequest
  , ''RestrictChatMemberRequest
  , ''UnbanChatMemberRequest
  , ''BanChatMemberRequest
  , ''GetUserProfilePhotosRequest
  , ''SendDiceRequest
  , ''SendPollRequest
  , ''SendContactRequest
  , ''SendVenueRequest
  , ''StopMessageLiveLocationRequest
  , ''EditMessageLiveLocationRequest
  , ''SendLocationRequest
  , ''CopyMessageRequest
  ]