{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Telegram.Bot.Simple where

import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Bifunctor
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (traverse)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics (Generic)
import Servant.Client

import Telegram.Bot.API

-- | Bot handler context.
--
-- The context may include an 'Update' the bot is handling at the moment.
newtype BotM a = BotM { _runBotM :: ReaderT (Maybe Update) ClientM a }
  deriving (Functor, Applicative, Monad, MonadBase IO, MonadReader (Maybe Update), MonadIO)

liftClientM :: ClientM a -> BotM a
liftClientM = BotM . lift

runBotM :: Maybe Update -> BotM a -> ClientM a
runBotM update = flip runReaderT update . _runBotM

-- | Get current 'ChatId' if possible.
currentChatId :: BotM (Maybe ChatId)
currentChatId = do
  mupdate <- ask
  pure $ do
    Update{..} <- mupdate
    Message{..} <- asum
      [ updateMessage
      , updateEditedMessage
      , updateChannelPost
      , updateEditedChannelPost
      , callbackMessage updateCallbackQuery
      ]
    pure $ chatId messageChat
  where
    callbackMessage mupdateCallbackQuery = case mupdateCallbackQuery of
      Just cb -> callbackQueryMessage cb
      Nothing -> Nothing

currentCallbackQueryId :: BotM (Maybe CallbackQueryId)
currentCallbackQueryId = do
  mupdate <- ask
  pure $ do
    Update{..} <- mupdate
    callbackQID updateCallbackQuery
  where
    callbackQID mupdateCallbackQuery = case mupdateCallbackQuery of
      Just cb -> Just (callbackQueryId cb)
      Nothing -> Nothing

newtype Eff action model = Eff { _runEff :: Writer [BotM action] model }
  deriving (Functor, Applicative, Monad)

instance Bifunctor Eff where
  bimap f g = Eff . mapWriter (bimap g (map (fmap f))) . _runEff

runEff :: Eff action model -> (model, [BotM action])
runEff = runWriter . _runEff

eff :: BotM a -> Eff a ()
eff e = Eff (tell [e])

(<#) :: model -> BotM action -> Eff action model
m <# a = eff a >> pure m

data BotApp model action = BotApp
  { botInitialModel :: model
  , botAction       :: Update -> model -> Maybe action
  , botHandler      :: action -> model -> Eff action model
  , botJobs         :: [BotJob model action]
  }

data BotJob model action = BotJob
  { botJobSchedule :: Int
  , botJobTask     :: model -> ClientM model
  }

startBotAsync :: Show action => BotApp model action -> ClientEnv -> IO (action -> IO ())
startBotAsync bot env = do
  modelVar <- newTVarIO (botInitialModel bot)
  fork_ $ startBotPolling bot modelVar
  return undefined
  where
    fork_ = void . forkIO . void . flip runClientM env

startBotAsync_ :: Show action => BotApp model action -> ClientEnv -> IO ()
startBotAsync_ bot env = void (startBot bot env)

startBot :: Show action => BotApp model action -> ClientEnv -> IO (Either ServantError ())
startBot bot env = do
  modelVar <- newTVarIO (botInitialModel bot)
  runClientM (startBotPolling bot modelVar) env

startBot_ :: Show action => BotApp model action -> ClientEnv -> IO ()
startBot_ bot env = do 
  res <- startBot bot env
  case res of
    Right _  -> return ()
    Left err -> putStrLn (show err)

startBotPolling :: Show action => BotApp model action -> TVar model -> ClientM ()
startBotPolling BotApp{..} = startPolling . handleUpdate
  where
    handleUpdate modelVar update = void . liftBaseDiscard forkIO $
      handleAction' modelVar (Just update) (botAction update)
      `catchError` (liftIO . print) -- print error on failed update handlers

    handleAction' modelVar update toAction = do
      actions <- liftIO $ atomically $ do
        model <- readTVar modelVar
        case toAction model of
          Just action -> case runEff (botHandler action model) of
            (newModel, actions) -> do
              writeTVar modelVar newModel
              return actions
          Nothing -> return []
      acts <- liftIO $ atomically $ do
        model <- readTVar modelVar
        return $ toAction model
      liftIO $ putStrLn $ show acts
      mapM_ ((>>= handleAction' modelVar update . const . Just) . runBotM update) actions

startPolling :: (Update -> ClientM ()) -> ClientM ()
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (UpdateId n) = UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <- getUpdates (GetUpdatesRequest offset Nothing Nothing Nothing)
      let updates = responseResult res
          updateIds = map updateUpdateId updates
          maxUpdateId = maximum (Nothing : map Just updateIds)
      mapM_ handleUpdate updates
      liftIO $ threadDelay 1000000
      go maxUpdateId

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

reply :: ReplyMessage -> BotM ()
reply rmsg = do
  mchatId <- currentChatId
  case mchatId of
    Just chatId -> do
      let msg = replyMessageToSendMessageRequest (SomeChatId chatId) rmsg
      void $ liftClientM $ sendMessage msg
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"

replyText :: Text -> BotM ()
replyText = reply . toReplyMessage

-- | Reply editMessage parameters.
-- This is just like 'EditMessageTextRequest' but without 'SomeChatId' specified.
data ReplyEditMessageText = ReplyEditMessageText
  { replyEditMessageTextMessageId             :: Maybe MessageId       -- ^ Required if inline_message_id is not specified. Identifier of the sent message
  , replyEditMessageTextInlineMessageId       :: Maybe Text            -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message
  , replyEditMessageTextText                  :: Text                  -- ^ New text of the message
  , replyEditMessageTextParseMode             :: Maybe ParseMode       -- ^ Send Markdown or HTML, if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , replyEditMessageTextDisableWebPagePreview :: Maybe Bool            -- ^ Disables link previews for links in this message
  , replyEditMessageTextReplyMarkup           :: Maybe SomeReplyMarkup -- ^ A JSON-serialized object for an inline keyboard
  } deriving (Generic)

toReplyEditMessageText :: MessageId -> Text -> Maybe SomeReplyMarkup -> ReplyEditMessageText
toReplyEditMessageText mId text srm = ReplyEditMessageText (Just mId) Nothing text Nothing Nothing srm

replyEditMessageTextToEditMessageTextRequest :: Maybe SomeChatId -> ReplyEditMessageText -> EditMessageTextRequest
replyEditMessageTextToEditMessageTextRequest someChatId ReplyEditMessageText{..} = EditMessageTextRequest
  { editMessageTextChatId                = someChatId
  , editMessageTextMessageId             = replyEditMessageTextMessageId
  , editMessageTextInlineMessageId       = replyEditMessageTextInlineMessageId
  , editMessageTextText                  = replyEditMessageTextText
  , editMessageTextParseMode             = replyEditMessageTextParseMode
  , editMessageTextDisableWebPagePreview = replyEditMessageTextDisableWebPagePreview
  , editMessageTextReplyMarkup           = replyEditMessageTextReplyMarkup
  }

replyEdit :: ReplyEditMessageText -> BotM ()
replyEdit rmsg = do
  mchatId <- currentChatId
  case mchatId of
    Just chatId -> do
      let msg = replyEditMessageTextToEditMessageTextRequest (Just (SomeChatId chatId)) rmsg
      void $ liftClientM $ editMessageText msg
    Nothing -> do
      liftIO $ putStrLn "No chat to reply to"


replyEditMessageText :: MessageId -> Text -> Maybe SomeReplyMarkup -> BotM()
replyEditMessageText mId text srm = replyEdit $ toReplyEditMessageText mId text srm

updateMessageText :: Update -> Maybe Text
updateMessageText = updateMessage >=> messageText


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


conversationBot
  :: (Eq conversation, Hashable conversation)
  => (Update -> Maybe conversation)
  -> BotApp (Maybe conversation, model) action
  -> BotApp (HashMap conversation (Maybe conversation, model)) (conversation, action)
conversationBot toConversation BotApp{..} = BotApp
  { botInitialModel = conversationInitialModel
  , botAction       = conversationAction
  , botHandler      = conversationHandler
  , botJobs         = conversationJobs
  }
  where
    conversationInitialModel = HashMap.empty

    conversationAction update conversations = do
      conversation <- toConversation update
      let (_, model) = fromMaybe botInitialModel (HashMap.lookup conversation conversations)
      (conversation,) <$> botAction update (Just conversation, model)

    conversationHandler (conversation, action) conversations =
      bimap (conversation,) (\m -> HashMap.insert conversation m conversations) $
        botHandler action (Just conversation, model)
      where
        (_, model) = fromMaybe botInitialModel (HashMap.lookup conversation conversations)

    conversationJobs = map toConversationJob botJobs

    toConversationJob BotJob{..} = BotJob
      { botJobSchedule = botJobSchedule
      , botJobTask = traverse botJobTask
      }

urlButton :: Text -> Text -> InlineKeyboardButton
urlButton label url = (labeledInlineKeyboardButton label) { inlineKeyboardButtonUrl = Just url}

callbackButton :: Text -> Text -> InlineKeyboardButton
callbackButton label data_ = (labeledInlineKeyboardButton label) { inlineKeyboardButtonCallbackData = Just data_}

actionButton :: Show action => Text -> action -> InlineKeyboardButton
actionButton label action = callbackButton label (Text.pack (show action))