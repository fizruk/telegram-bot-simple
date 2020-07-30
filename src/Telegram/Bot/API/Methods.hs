{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
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
import Servant.Multipart
import System.FilePath

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

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

data DocumentFile
  = DocumentFileId Int
  | DocumentUrl Text
  | DocumentFile FilePath ContentType

instance ToJSON DocumentFile where
  toJSON (DocumentFileId i) = toJSON (show i)
  toJSON (DocumentUrl t) = toJSON t
  toJSON (DocumentFile f _) = toJSON ("attach://" <> T.pack (takeFileName f))

type ContentType = Text

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
        $ (maybe id (\t -> ((Input "disable_notifications" (bool "false" "true" t)):)) sendDocumentDisableNotification)
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
