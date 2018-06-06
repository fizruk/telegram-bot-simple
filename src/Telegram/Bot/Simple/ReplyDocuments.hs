{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Telegram.Bot.Simple.ReplyDocuments where

import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Network.Mime

import Telegram.Bot.API
import Telegram.Bot.Simple.Eff
import Telegram.Bot.Simple.Reply

-- ** 'ReplyPhoto'

data ReplyPhoto payload = ReplyPhoto
  { replyPhotoPhoto                :: payload -- ^ Photo to send. You can either pass a file_id as String to resend a photo that is already on the Telegram servers, or upload a new photo.
  , replyPhotoCaption              :: Maybe Text -- ^ Photo caption (may also be used when resending photos by file_id), 0-200 characters.
  , replyPhotoParseMode            :: Maybe ParseMode -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in the media caption.
  , replyPhotoDisableNotification  :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , replyPhotoReplyToMessageId     :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , replyPhotoReplyMarkup          :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } 
  deriving (Generic)

toReplyPhoto :: FilePath -> ReplyPhoto FileUpload
toReplyPhoto path = ReplyPhoto (FileUpload (Just $ defaultMimeLookup $ Text.pack path) (FileUploadFile path)) Nothing Nothing Nothing Nothing Nothing

replyPhotoToSendPhotoRequest :: SomeChatId -> ReplyPhoto FileUpload -> SendPhotoRequest FileUpload
replyPhotoToSendPhotoRequest someChatId ReplyPhoto{..} = SendPhotoRequest
  { sendPhotoChatId              = someChatId
  , sendPhotoPhoto               = replyPhotoPhoto
  , sendPhotoCaption             = replyPhotoCaption
  , sendPhotoParseMode           = replyPhotoParseMode
  , sendPhotoDisableNotification = replyPhotoDisableNotification
  , sendPhotoReplyToMessageId    = replyPhotoReplyToMessageId
  , sendPhotoReplyMarkup         = replyPhotoReplyMarkup
  } 

replyReplyPhoto :: ReplyPhoto FileUpload -> BotM ()
replyReplyPhoto rph = do
  phchatId <- currentChatId
  case phchatId of
    Just chatId -> do
      let photo = replyPhotoToSendPhotoRequest (SomeChatId chatId) rph
      void $ liftClientM $ sendPhoto photo
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"

replyPhoto :: FilePath -> BotM ()
replyPhoto path = replyReplyPhoto $ toReplyPhoto path


-- ** 'ReplyAudio'

data ReplyAudio payload = ReplyAudio
  { replyAudioAudio               :: payload -- ^ Audio to send. You can either pass a file_id as String to resend an audio that is already on the Telegram servers, or upload a new audio file.
  , replyAudioCaption             :: Maybe Text -- ^ Audio caption, 0-200 characters
  , replyAudioParseMode           :: Maybe ParseMode -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in the media caption.
  , replyAudioDuration            :: Maybe Int -- ^ Duration of the audio in seconds
  , replyAudioPerformer           :: Maybe Text -- ^ Performer
  , replyAudioTitle               :: Maybe Text -- ^ Track name
  , replyAudioDisableNotification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , replyAudioReplyToMessageId    :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , replyAudioReplyMarkup         :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } 
  deriving (Generic)

toReplyAudio :: FilePath -> ReplyAudio FileUpload
toReplyAudio path = ReplyAudio (FileUpload (Just $ defaultMimeLookup $ Text.pack path) (FileUploadFile path)) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

replyAudioToSendAudioRequest :: SomeChatId -> ReplyAudio FileUpload -> SendAudioRequest FileUpload
replyAudioToSendAudioRequest someChatId ReplyAudio{..} = SendAudioRequest
  { sendAudioChatId              = someChatId
  , sendAudioAudio               = replyAudioAudio
  , sendAudioCaption             = replyAudioCaption
  , sendAudioParseMode           = replyAudioParseMode
  , sendAudioDuration            = replyAudioDuration
  , sendAudioPerformer           = replyAudioPerformer
  , sendAudioTitle               = replyAudioTitle
  , sendAudioDisableNotification = replyAudioDisableNotification
  , sendAudioReplyToMessageId    = replyAudioReplyToMessageId
  , sendAudioReplyMarkup         = replyAudioReplyMarkup
  } 

replyReplyAudio :: ReplyAudio FileUpload -> BotM ()
replyReplyAudio rau = do
  achatId <- currentChatId
  case achatId of
    Just chatId -> do
      let audio = replyAudioToSendAudioRequest (SomeChatId chatId) rau
      void $ liftClientM $ sendAudio audio
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"

replyAudio :: FilePath -> BotM ()
replyAudio path = replyReplyAudio $ toReplyAudio path


-- ** 'ReplyDocument'

data ReplyDocument payload = ReplyDocument
  { replyDocumentDocument            :: payload -- ^ File to send. Pass a file_id as String to send a file that exists on the Telegram servers (recommended), pass an HTTP URL as a String for Telegram to get a file from the Internet, or upload a new one using multipart/form-data.
  , replyDocumentCaption             :: Maybe Text -- ^ Document caption, 0-200 characters
  , replyDocumentParseMode           :: Maybe ParseMode -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in the media caption.
  , replyDocumentDisableNotification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , replyDocumentReplyToMessageId    :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , replyDocumentReplyMarkup         :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } 
  deriving (Generic)

toReplyDocument :: FilePath -> ReplyDocument FileUpload
toReplyDocument path = ReplyDocument (FileUpload (Just $ defaultMimeLookup $ Text.pack path) (FileUploadFile path)) Nothing Nothing Nothing Nothing Nothing

replyDocumentToSendDocumentRequest :: SomeChatId -> ReplyDocument FileUpload -> SendDocumentRequest FileUpload
replyDocumentToSendDocumentRequest someChatId ReplyDocument{..} = SendDocumentRequest
  { sendDocumentChatId              = someChatId
  , sendDocumentDocument            = replyDocumentDocument
  , sendDocumentCaption             = replyDocumentCaption
  , sendDocumentParseMode           = replyDocumentParseMode
  , sendDocumentDisableNotification = replyDocumentDisableNotification
  , sendDocumentReplyToMessageId    = replyDocumentReplyToMessageId
  , sendDocumentReplyMarkup         = replyDocumentReplyMarkup
  } 

replyReplyDocument :: ReplyDocument FileUpload -> BotM ()
replyReplyDocument rdoc = do
  docchatId <- currentChatId
  case docchatId of
    Just chatId -> do
      let document = replyDocumentToSendDocumentRequest (SomeChatId chatId) rdoc
      void $ liftClientM $ sendDocument document
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"

replyDocument :: FilePath -> BotM ()
replyDocument path = replyReplyDocument $ toReplyDocument path
