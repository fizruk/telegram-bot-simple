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

currentCallbackQueryId :: BotM (Maybe CallbackQueryId)
currentCallbackQueryId = do
  mupdate <- asks botContextUpdate
  pure $ do
    Update{..} <- mupdate
    callbackQID updateCallbackQuery
  where
    callbackQID mupdateCallbackQuery = case mupdateCallbackQuery of
      Just cb -> Just (callbackQueryId cb)
      Nothing -> Nothing

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



-- | Reply answerCallbackQuery parameters.
-- This is just like 'AnswerCallbackQueryRequest' but without 'CallbackQueryId' specified.
data ReplyAnswerCallbackQuery = ReplyAnswerCallbackQuery
  { replyAnswerCallbackQueryText            :: Maybe Text -- | ^ Text of the notification. If not specified, nothing will be shown to the user, 0-200 characters
  , replyAnswerCallbackQueryShowAlert       :: Maybe Bool -- | ^ If true, an alert will be shown by the client instead of a notification at the top of the chat screen. Defaults to false.
  , replyAnswerCallbackQueryUrl             :: Maybe Text -- | ^ URL that will be opened by the user's client. If you have created a Game and accepted the conditions via @Botfather, specify the URL that opens your game – note that this will only work if the query comes from a callback_game button. Otherwise, you may use links like t.me/your_bot?start=XXXX that open your bot with a parameter.
  , replyAnswerCallbackQueryCacheTime       :: Maybe Int  -- | ^ The maximum amount of time in seconds that the result of the callback query may be cached client-side. Telegram apps will support caching starting in version 3.14. Defaults to 0.
  } deriving (Generic)

toReplyAnswerCallbackQuery :: Maybe Text -> ReplyAnswerCallbackQuery
toReplyAnswerCallbackQuery text = ReplyAnswerCallbackQuery text Nothing Nothing Nothing

replyAnswerCBQToAnswerCBQRequest :: CallbackQueryId -> ReplyAnswerCallbackQuery -> AnswerCallbackQueryRequest
replyAnswerCBQToAnswerCBQRequest callbackQueryId ReplyAnswerCallbackQuery{..} = AnswerCallbackQueryRequest
  { answerCallbackQueryCallbackQueryId = callbackQueryId
  , answerCallbackQueryText            = replyAnswerCallbackQueryText
  , answerCallbackQueryShowAlert       = replyAnswerCallbackQueryShowAlert
  , answerCallbackQueryUrl             = replyAnswerCallbackQueryUrl
  , answerCallbackQueryCacheTime       = replyAnswerCallbackQueryCacheTime
  }

replyCallbackQueryRequest :: ReplyAnswerCallbackQuery -> BotM ()
replyCallbackQueryRequest racb = do
  mcbId <- currentCallbackQueryId
  case mcbId of
    Just cbId -> do
      let cBQ = replyAnswerCBQToAnswerCBQRequest cbId racb
      void $ liftClientM $ answerCallbackQuery cBQ
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"


replyCallbackQuery :: Maybe Text -> BotM()
replyCallbackQuery text = replyCallbackQueryRequest $ toReplyAnswerCallbackQuery text 

