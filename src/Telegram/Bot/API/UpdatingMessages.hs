{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.UpdatingMessages where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Servant.API
import           Servant.Client                  (ClientM, client)

import           Telegram.Bot.API.Internal.Utils (deriveJSON', gtoJSON)
import           Telegram.Bot.API.MakingRequests
import           Telegram.Bot.API.Methods
import           Telegram.Bot.API.Types

data EditMessageResponse
  = EditedInlineMessage Bool
  | EditedMessage Message
  deriving (Show, Generic)

instance FromJSON EditMessageResponse where
  parseJSON (Data.Aeson.Bool b) = pure (EditedInlineMessage b)
  parseJSON o@(Data.Aeson.Object _) = EditedMessage <$> parseJSON o
  parseJSON _ = fail "Unable to parse EditMessageResponse: expected either a Bool or a Message"

-- ** 'editMessageText'

-- | Request parameters for 'editMessageText'.
data EditMessageTextRequest = EditMessageTextRequest
  { editMessageTextChatId                :: Maybe SomeChatId -- ^ Required if 'editMessageTextInlineMessageId' is not specified. Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , editMessageTextMessageId             :: Maybe MessageId -- ^ Required if 'editMessageTextInlineMessageId' is not specified. Identifier of the sent message.
  , editMessageTextInlineMessageId       :: Maybe MessageId -- ^ Required if 'editMessageTextChatId' and 'editMessageTextMessageId' are not specified. Identifier of the sent message.
  , editMessageTextText                  :: Text -- ^ Text of the message to be sent.
  , editMessageTextParseMode             :: Maybe ParseMode -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , editMessageEntities                  :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in message text, which can be specified instead of /parse_mode/.
  , editMessageTextDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , editMessageTextReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
} deriving (Generic)

-- | Request parameters for 'editMessageCaption'.
data EditMessageCaptionRequest = EditMessageCaptionRequest
  { editMessageCaptionChatId           :: Maybe SomeChatId -- ^ Required if 'editMessageCaptionMessageId' is not specified. Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , editMessageCaptionMessageId        :: Maybe MessageId -- ^ Required if 'editMessageCaptionInlineMessageId' is not specified. Identifier of the sent message.
  , editMessageCaptionInlineMessageId  :: Maybe MessageId -- ^ Required if 'editMessageCaptionChatId' and 'editMessageCaptionMessageId' are not specified. Identifier of the sent message.
  , editMessageCaptionCaption          :: Maybe Text -- ^ New caption of the message, 0-1024 characters after entities parsing
  , editMessageCaptionParseMode        :: Maybe ParseMode -- ^ Mode for parsing entities in the message caption. See formatting options for more details.
  , editMessageCaptionCaptionEntities  :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , editMessageCaptionReplyMarkup      :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
} deriving (Generic)

-- | Request parameters for 'editMessageMedia'.
data EditMessageMediaRequest = EditMessageMediaRequest
  { editMessageMediaChatId           :: Maybe SomeChatId -- ^ Required if 'editMessageMediaMessageId' is not specified. Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , editMessageMediaMessageId        :: Maybe MessageId -- ^ Required if 'editMessageMediaInlineMessageId' is not specified. Identifier of the sent message.
  , editMessageMediaInlineMessageId  :: Maybe MessageId -- ^ Required if 'editMessageMediaChatId' and 'editMessageMediaMessageId' are not specified. Identifier of the sent message.
  , editMessageMediaMedia            :: InputMedia -- ^ A JSON-serialized object for a new media content of the message
  , editMessageMediaReplyMarkup      :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
} deriving (Generic)

instance ToJSON EditMessageMediaRequest where toJSON = gtoJSON

-- | Request parameters for 'editMessageReplyMarkup'.
data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest
  { editMessageReplyMarkupChatId           :: Maybe SomeChatId -- ^ Required if 'editMessageReplyMarkupMessageId' is not specified. Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , editMessageReplyMarkupMessageId        :: Maybe MessageId -- ^ Required if 'editMessageReplyMarkupInlineMessageId' is not specified. Identifier of the sent message.
  , editMessageReplyMarkupInlineMessageId  :: Maybe MessageId -- ^ Required if 'editMessageReplyMarkupChatId' and 'editMessageReplyMarkupMessageId' are not specified. Identifier of the sent message.
  , editMessageReplyMarkupReplyMarkup      :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
} deriving (Generic)

-- | Request parameters for 'stopPoll'.
data StopPollRequest = StopPollRequest
  { stopPollChatId           :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , stopPollMessageId        :: MessageId -- ^ Identifier of the original message with the poll
  , stopPollReplyMarkup      :: Maybe SomeReplyMarkup -- ^ A JSON-serialized object for a new message inline keyboard.
  } deriving (Generic)

foldMap deriveJSON' 
  [ ''EditMessageTextRequest
  , ''EditMessageCaptionRequest
  , ''EditMessageReplyMarkupRequest
  , ''StopPollRequest
  ]


type EditMessageText
  = "editMessageText"
  :> ReqBody '[JSON] EditMessageTextRequest
  :> Post '[JSON] (Response EditMessageResponse)

-- | Use this method to edit text and game messages. On success, if the edited message is not an inline message, the edited 'Message' is returned, otherwise 'True' is returned.
editMessageText :: EditMessageTextRequest -> ClientM (Response EditMessageResponse)
editMessageText = client (Proxy @EditMessageText)

type EditMessageCaption  = "editMessageCaption"
  :> ReqBody '[JSON] EditMessageCaptionRequest
  :> Post '[JSON] (Response EditMessageResponse)

-- | Use this method to edit captions of messages.
--   On success, if the edited message is not an
--   inline message, the edited Message is returned,
--   otherwise True is returned.
editMessageCaption :: EditMessageCaptionRequest -> ClientM (Response EditMessageResponse)
editMessageCaption = client (Proxy @EditMessageCaption)

type EditMessageMedia  = "editMessageMedia"
  :> ReqBody '[JSON] EditMessageMediaRequest
  :> Post '[JSON] (Response EditMessageResponse)

-- | Use this method to edit animation, audio,
--   document, photo, or video messages. If a
--   message is part of a message album, then it
--   can be edited only to an audio for audio albums,
--   only to a document for document albums and to a
--   photo or a video otherwise. When an inline message
--   is edited, a new file can't be uploaded; use a
--   previously uploaded file via its file_id or specify a URL.
--   On success, if the edited message is not an inline
--   message, the edited Message is returned, otherwise True is returned.
editMessageMedia :: EditMessageMediaRequest -> ClientM (Response EditMessageResponse)
editMessageMedia = client (Proxy @EditMessageMedia)


type EditMessageReplyMarkup = "editMessageReplyMarkup"
  :> ReqBody '[JSON] EditMessageReplyMarkupRequest
  :> Post '[JSON] (Response EditMessageResponse)

-- | Use this method to edit only the reply markup of messages.
--   On success, if the edited message is not an inline message,
--   the edited Message is returned, otherwise True is returned.
editMessageReplyMarkup :: EditMessageReplyMarkupRequest -> ClientM (Response EditMessageResponse)
editMessageReplyMarkup = client (Proxy @EditMessageReplyMarkup)

type StopPoll = "stopPoll"
  :> ReqBody '[JSON] StopPollRequest
  :> Post '[JSON] (Response Poll)

-- | Use this method to stop a poll which was sent by the bot.
--   On success, the stopped Poll is returned.
stopPoll :: StopPollRequest -> ClientM (Response Poll)
stopPoll = client (Proxy @StopPoll)


