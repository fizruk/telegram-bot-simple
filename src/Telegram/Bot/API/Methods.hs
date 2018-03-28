{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods where


import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.MultipartFormData
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Mime
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.MultipartFormData
import Telegram.Bot.API.Types

-- * Available methods

-- ** 'getMe'

type GetMe = "getMe" :> Get '[JSON] (Response User)

-- | A simple method for testing your bot's auth token.
-- Requires no parameters.
-- Returns basic information about the bot in form of a 'User' object.
getMe :: ClientM (Response User)
getMe = client (Proxy @GetMe)

-- ** 'sendMessage'

type SendMessage
  = "sendMessage" :> ReqBody '[JSON] SendMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
sendMessage :: SendMessageRequest -> ClientM (Response Message)
sendMessage = client (Proxy @SendMessage)

-- | Unique identifier for the target chat
-- or username of the target channel (in the format @\@channelusername@).
data SomeChatId
  = SomeChatId ChatId       -- ^ Unique chat ID.
  | SomeChatUsername Text   -- ^ Username of the target channel.
  deriving (Generic)

instance ToJSON   SomeChatId where toJSON = genericSomeToJSON
instance FromJSON SomeChatId where parseJSON = genericSomeParseJSON

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

-- | This object represents data (image, video, ...) to upload.
data FileUploadContent =
    FileUploadFile FilePath
  | FileUploadBS BS.ByteString
  | FileUploadLBS LBS.ByteString
  deriving (Show)

-- | This object represents data (image, video, ...) with mime type to upload.
data FileUpload = FileUpload
  { fileUpload_type    :: Maybe MimeType    -- ^ Mime type of the upload.
  , fileUpload_content :: FileUploadContent -- ^ The payload/source to upload.
  } 
  deriving (Show)

-- ** 'sendPhoto'

type SendPhoto 
  = "sendPhoto" :> MultipartFormDataReqBody (SendPhotoRequest FileUpload) :> Post '[JSON] (Response Message)

-- | Use this method to send photos. 
-- On success, the sent Message is returned.

sendPhoto :: SendPhotoRequest FileUpload -> ClientM (Response Message)
sendPhoto = client (Proxy @SendPhoto)

data SendPhotoRequest payload = SendPhotoRequest
  { sendPhotoChatId              :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @@channelusername@)
  , sendPhotoPhoto               :: payload -- ^ Photo to send. You can either pass a file_id as String to resend a photo that is already on the Telegram servers, or upload a new photo.
  , sendPhotoCaption             :: Maybe Text -- ^ Photo caption (may also be used when resending photos by file_id), 0-200 characters.
  , sendPhotoDisableNotification :: Maybe Bool -- ^ Sends the message silently. iOS users will not receive a notification, Android users will receive a notification with no sound.
  , sendPhotoReplyToMessageId    :: Maybe Int -- ^ If the message is a reply, ID of the original message
  , sendPhotoReplyMarkup         :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for a custom reply keyboard, instructions to hide keyboard or to force a reply from the user.
  } 
  deriving (Generic)

uploadPhotoRequest :: SomeChatId -> FileUpload -> SendPhotoRequest FileUpload
uploadPhotoRequest chatId_ photo = SendPhotoRequest chatId_ photo Nothing Nothing Nothing Nothing

utf8Part :: Text -> Text -> Part
utf8Part inputName = partBS inputName . T.encodeUtf8

chatIdToPart :: SomeChatId -> Text
chatIdToPart (SomeChatId chId)    = case chId of
  ChatId integer -> tshow integer
chatIdToPart (SomeChatUsername text) = tshow text

fileUploadToPart :: Text -> FileUpload -> Part
fileUploadToPart inputName fileUpload =
  let part =
        case fileUpload_content fileUpload of
          FileUploadFile path -> partFileSource inputName path
          FileUploadBS bs     -> partBS inputName bs
          FileUploadLBS lbs   -> partLBS inputName lbs
  in part { partContentType = fileUpload_type fileUpload }

instance ToMultipartFormData (SendPhotoRequest FileUpload) where
  toMultipartFormData req =
    [ utf8Part (T.pack "chat_id") (chatIdToPart $ sendPhotoChatId req) ] ++
    catMaybes
    [ utf8Part (T.pack "caption") <$> sendPhotoCaption req
    , partLBS (T.pack "disable_notification") . encode <$> sendPhotoDisableNotification req
    , utf8Part (T.pack "reply_to_message_id") . tshow <$> sendPhotoReplyToMessageId req
    , partLBS (T.pack "reply_markup") . encode <$> sendPhotoReplyMarkup req
    ] ++
    [ fileUploadToPart (T.pack "photo") (sendPhotoPhoto req) ]

tshow :: Show a => a -> Text
tshow = T.pack . show
