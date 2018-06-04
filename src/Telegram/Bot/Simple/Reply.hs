{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Bot.Simple.Reply where

import           Control.Applicative     ((<|>))
import           Control.Monad.Reader
import           Data.String
import           Data.Text               (Text)
import           GHC.Generics            (Generic)

import           Telegram.Bot.API        as Telegram
import           Telegram.Bot.Simple.Eff

-- | Get current 'ChatId' if possible.
currentChatId :: BotM (Maybe ChatId)
currentChatId = do
  mupdate <- asks botContextUpdate
  pure $ updateChatId =<< mupdate

getEditMessageId :: BotM (Maybe EditMessageId)
getEditMessageId = do
  mupdate <- asks botContextUpdate
  pure $ updateEditMessageId =<< mupdate

updateEditMessageId :: Update -> Maybe EditMessageId
updateEditMessageId update
    = EditInlineMessageId
      <$> (callbackQueryInlineMessageId =<< updateCallbackQuery update)
  <|> EditChatMessageId
      <$> (SomeChatId . chatId . messageChat <$> message)
      <*> (messageMessageId <$> message)
  where
    message = extractUpdateMessage update

-- | Reply message parameters.
-- This is just like 'SendMessageRequest' but without 'SomeChatId' specified.
data ReplyMessage = ReplyMessage
  { replyMessageText                  :: Text -- ^ Text of the message to be sent.
  , replyMessageParseMode             :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , replyMessageDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , replyMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , replyMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , replyMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

instance IsString ReplyMessage where
  fromString = toReplyMessage . fromString

-- | Create a 'ReplyMessage' with just some 'Text' message.
toReplyMessage :: Text -> ReplyMessage
toReplyMessage text = ReplyMessage text Nothing Nothing Nothing Nothing Nothing

replyMessageToSendMessageRequest :: SomeChatId -> ReplyMessage -> SendMessageRequest
replyMessageToSendMessageRequest someChatId ReplyMessage{..} = SendMessageRequest
  { sendMessageChatId = someChatId
  , sendMessageText = replyMessageText
  , sendMessageParseMode = replyMessageParseMode
  , sendMessageDisableWebPagePreview = replyMessageDisableWebPagePreview
  , sendMessageDisableNotification = replyMessageDisableNotification
  , sendMessageReplyToMessageId = replyMessageReplyToMessageId
  , sendMessageReplyMarkup = replyMessageReplyMarkup
  }

-- | Reply in a chat with a given 'SomeChatId'.
replyTo :: SomeChatId -> ReplyMessage -> BotM ()
replyTo someChatId rmsg = do
  let msg = replyMessageToSendMessageRequest someChatId rmsg
  void $ liftClientM $ sendMessage msg

-- | Reply in the current chat (if possible).
reply :: ReplyMessage -> BotM ()
reply rmsg = do
  mchatId <- currentChatId
  case mchatId of
    Just chatId -> replyTo (SomeChatId chatId) rmsg
    Nothing     -> liftIO $ putStrLn "No chat to reply to"

-- | Reply with a text.
replyText :: Text -> BotM ()
replyText = reply . toReplyMessage

data EditMessage = EditMessage
  { editMessageText                  :: Text
  , editMessageParseMode             :: Maybe ParseMode
  , editMessageDisableWebPagePreview :: Maybe Bool
  , editMessageReplyMarkup           :: Maybe SomeReplyMarkup
  }

instance IsString EditMessage where
  fromString = toEditMessage . fromString

data EditMessageId
  = EditChatMessageId SomeChatId MessageId
  | EditInlineMessageId MessageId

toEditMessage :: Text -> EditMessage
toEditMessage msg = EditMessage msg Nothing Nothing Nothing

editMessageToEditMessageTextRequest
  :: EditMessageId -> EditMessage -> EditMessageTextRequest
editMessageToEditMessageTextRequest editMessageId EditMessage{..}
  = EditMessageTextRequest
    { editMessageTextText = editMessageText
    , editMessageTextParseMode = editMessageParseMode
    , editMessageTextDisableWebPagePreview = editMessageDisableWebPagePreview
    , editMessageTextReplyMarkup = editMessageReplyMarkup
    , ..
    }
  where
    ( editMessageTextChatId,
      editMessageTextMessageId,
      editMessageTextInlineMessageId )
      = case editMessageId of
          EditChatMessageId chatId messageId
            -> (Just chatId, Just messageId, Nothing)
          EditInlineMessageId messageId
            -> (Nothing, Nothing, Just messageId)

editMessageToReplyMessage :: EditMessage -> ReplyMessage
editMessageToReplyMessage EditMessage{..} = (toReplyMessage editMessageText)
  { replyMessageParseMode = editMessageParseMode
  , replyMessageDisableWebPagePreview = editMessageDisableWebPagePreview
  , replyMessageReplyMarkup = editMessageReplyMarkup
  }

editMessage :: EditMessageId -> EditMessage -> BotM ()
editMessage editMessageId emsg = do
  let msg = editMessageToEditMessageTextRequest editMessageId emsg
  void $ liftClientM $ Telegram.editMessageText msg

editUpdateMessage :: EditMessage -> BotM ()
editUpdateMessage emsg = do
  mEditMessageId <- getEditMessageId
  case mEditMessageId of
    Just editMessageId -> editMessage editMessageId emsg
    Nothing            -> liftIO $ putStrLn "Can't find message to edit!"

editUpdateMessageText :: Text -> BotM ()
editUpdateMessageText = editUpdateMessage . toEditMessage

replyOrEdit :: EditMessage -> BotM ()
replyOrEdit emsg = do
  uid <- asks (fmap userId . (messageFrom =<<) . (extractUpdateMessage =<<) . botContextUpdate)
  botUserId <- asks (userId . botContextUser)
  if uid == Just botUserId
     then editUpdateMessage emsg
     else reply (editMessageToReplyMessage emsg)
