{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.UpdatingMessages where

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'editMessageText'

type EditMessageText
  = "editMessageText" :> ReqBody '[JSON] EditMessageTextRequest :> Post '[JSON] (Response Message)

-- ** 'EditMessageTextRequest'

data EditMessageTextRequest = EditMessageTextRequest 
  { editMessageTextChatId                :: Maybe SomeChatId      -- ^ Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editMessageTextMessageId             :: Maybe MessageId       -- ^ Required if inline_message_id is not specified. Identifier of the sent message
  , editMessageTextInlineMessageId       :: Maybe Text            -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , editMessageTextText                  :: Text                  -- ^ New text of the message
  , editMessageTextParseMode             :: Maybe ParseMode       -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , editMessageTextDisableWebPagePreview :: Maybe Bool            -- ^ Disables link previews for links in this message
  , editMessageTextReplyMarkup           :: Maybe SomeReplyMarkup -- ^ A JSON-serialized object for an inline keyboard
  } deriving (Generic)

instance ToJSON   EditMessageTextRequest where toJSON = gtoJSON
instance FromJSON EditMessageTextRequest where parseJSON = gparseJSON

-- | Use this method to edit text and game messages sent by the bot or via the bot (for inline bots). 
-- On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.
editMessageText :: EditMessageTextRequest -> ClientM (Response Message)
editMessageText = client (Proxy @EditMessageText)

uploadEditMessageTextRequest :: SomeChatId -> MessageId -> Text -> EditMessageTextRequest
uploadEditMessageTextRequest chatId_ messId msg = EditMessageTextRequest (Just chatId_) (Just messId) Nothing msg Nothing Nothing Nothing

-- ** 'editMessageCaption'

type EditMessageCaption 
  = "editMessageCaption" :> ReqBody '[JSON] EditMessageCaptionRequest :> Post '[JSON] (Response Message)
  
-- ** 'EditMessageCaptionRequest'

data EditMessageCaptionRequest = EditMessageCaptionRequest 
  { editMessageCaptionChatId                :: Maybe SomeChatId      -- ^ Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editMessageCaptionMessageId             :: Maybe MessageId       -- ^ Required if inline_message_id is not specified. Identifier of the sent message
  , editMessageCaptionInlineMessageId       :: Maybe Text            -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , editMessageCaptionCaption               :: Text                  -- ^ New caption of the message
  , editMessageCaptionParseMode             :: Maybe ParseMode       -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , editMessageCaptionReplyMarkup           :: Maybe SomeReplyMarkup -- ^ A JSON-serialized object for an inline keyboard
  } deriving (Generic)

instance ToJSON   EditMessageCaptionRequest where toJSON = gtoJSON
instance FromJSON EditMessageCaptionRequest where parseJSON = gparseJSON

-- | Use this method to edit captions of messages sent by the bot or via the bot (for inline bots). 
-- On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.
editMessageCaption :: EditMessageCaptionRequest -> ClientM (Response Message)
editMessageCaption = client (Proxy @EditMessageCaption)

uploadEditMessageCaptionRequest :: SomeChatId -> MessageId -> Text -> EditMessageCaptionRequest
uploadEditMessageCaptionRequest chatId_ messId caption = EditMessageCaptionRequest (Just chatId_) (Just messId) Nothing caption Nothing Nothing

-- ** 'editMessageReplyMarkup'

type EditMessageReplyMarkup 
  = "editMessageReplyMarkup" :> ReqBody '[JSON] EditMessageReplyMarkupRequest :> Post '[JSON] (Response Message)

-- ** 'EditMessageReplyMarkupRequest'

data EditMessageReplyMarkupRequest = EditMessageReplyMarkupRequest 
  { editMessageReplyMarkupChatId                :: Maybe SomeChatId      -- ^ Required if inline_message_id is not specified. Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , editMessageReplyMarkupMessageId             :: Maybe MessageId       -- ^ Required if inline_message_id is not specified. Identifier of the sent message
  , editMessageReplyMarkupInlineMessageId       :: Maybe Text            -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , editMessageReplyMarkupReplyMarkup           :: Maybe SomeReplyMarkup -- ^ A JSON-serialized object for an inline keyboard
  } deriving (Generic)

instance ToJSON   EditMessageReplyMarkupRequest where toJSON = gtoJSON
instance FromJSON EditMessageReplyMarkupRequest where parseJSON = gparseJSON

-- | Use this method to edit only the reply markup of messages sent by the bot or via the bot (for inline bots). 
-- On success, if edited message is sent by the bot, the edited Message is returned, otherwise True is returned.
editMessageReplyMarkup :: EditMessageReplyMarkupRequest -> ClientM (Response Message)
editMessageReplyMarkup = client (Proxy @EditMessageReplyMarkup)

uploadEditMessageReplyMarkupRequest :: SomeChatId -> MessageId -> EditMessageReplyMarkupRequest
uploadEditMessageReplyMarkupRequest chatId_ messId = EditMessageReplyMarkupRequest (Just chatId_) (Just messId) Nothing Nothing


