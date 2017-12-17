{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Telegram.Bot.Simple where

import Control.Monad.Reader
import Control.Monad.Error.Class
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Bifunctor
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
  deriving (Functor, Applicative, Monad, MonadReader (Maybe Update), MonadIO)

liftClientM :: ClientM a -> BotM a
liftClientM = BotM . lift

runBotM :: Maybe Update -> BotM a -> ClientM a
runBotM update = flip runReaderT update . _runBotM

-- | Get current 'ChatId' if possible.
currentChatId :: BotM (Maybe ChatId)
currentChatId = do
  mupdate <- ask
  pure $ do
    update <- mupdate
    message <- updateMessage update
    pure $ chatId (messageChat message)

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

startBotAsync :: BotApp model action -> ClientEnv -> IO (action -> IO ())
startBotAsync bot env = do
  modelVar <- newTVarIO (botInitialModel bot)
  fork_ $ startBotPolling bot modelVar
  return undefined
  where
    fork_ = void . forkIO . void . flip runClientM env

startBotAsync_ :: BotApp model action -> ClientEnv -> IO ()
startBotAsync_ bot env = void (startBot bot env)

startBot :: BotApp model action -> ClientEnv -> IO (Either ServantError ())
startBot bot env = do
  modelVar <- newTVarIO (botInitialModel bot)
  runClientM (startBotPolling bot modelVar) env

startBot_ :: BotApp model action -> ClientEnv -> IO ()
startBot_ bot = void . startBot bot

startBotPolling :: BotApp model action -> TVar model -> ClientM ()
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

updateMessageText :: Update -> Maybe Text
updateMessageText = updateMessage >=> messageText

conversationBot
  :: (Eq conversation, Hashable conversation)
  => (Update -> Maybe conversation)
  -> BotApp model action
  -> BotApp (HashMap conversation model) (conversation, action)
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
      let model = fromMaybe botInitialModel (HashMap.lookup conversation conversations)
      (conversation,) <$> botAction update model

    conversationHandler (conversation, action) conversations =
      bimap (conversation,) (\m -> HashMap.insert conversation m conversations) $
        botHandler action model
      where
        model = fromMaybe botInitialModel (HashMap.lookup conversation conversations)

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
