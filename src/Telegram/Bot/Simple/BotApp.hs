{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Telegram.Bot.Simple.BotApp where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad           (forever, void)
import           Control.Monad.Except    (catchError)
import           Control.Monad.Trans     (liftIO)
import           Data.Bifunctor          (first)
import           Data.String             (fromString)
import           Data.Text               (Text)
import           Servant.Client          (ClientEnv, ClientM, ServantError,
                                          runClientM)
import qualified System.Cron             as Cron
import           System.Environment      (getEnv)

import qualified Telegram.Bot.API        as Telegram
import           Telegram.Bot.Simple.Eff

data BotApp model action = BotApp
  { botInitialModel :: model
  , botAction       :: Telegram.Update -> model -> Maybe action
  , botHandler      :: action -> model -> Eff action model
  , botJobs         :: [BotJob model action]
  }

data BotJob model action = BotJob
  { botJobSchedule :: Text                       -- ^ Cron schedule for the job.
  , botJobTask     :: model -> Eff action model  -- ^ Job function.
  }

data BotEnv model action = BotEnv
  { botModelVar     :: TVar model
  , botActionsQueue :: TQueue (Maybe Telegram.Update, action)
  }

instance Functor (BotJob model) where
  fmap f BotJob{..} = BotJob{ botJobTask = first f . botJobTask, .. }

runJobTask :: BotEnv model action -> ClientEnv -> (model -> Eff action model) -> IO ()
runJobTask botEnv@BotEnv{..} env task = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (task model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  res <- flip runClientM env $
    mapM_ ((>>= liftIO . issueAction botEnv Nothing) . runBotM Nothing) effects
  case res of
    Left err -> print err
    Right _  -> return ()

scheduleBotJob :: BotEnv model action -> ClientEnv -> BotJob model action -> IO [ThreadId]
scheduleBotJob botEnv env BotJob{..} = Cron.execSchedule $ do
  Cron.addJob (runJobTask botEnv env botJobTask) botJobSchedule

scheduleBotJobs :: BotEnv model action -> ClientEnv -> [BotJob model action] -> IO [ThreadId]
scheduleBotJobs botEnv env jobs = concat
  <$> traverse (scheduleBotJob botEnv env) jobs

defaultBotEnv :: BotApp model action -> IO (BotEnv model action)
defaultBotEnv BotApp{..} = BotEnv
  <$> newTVarIO botInitialModel
  <*> newTQueueIO

startBotAsync :: BotApp model action -> ClientEnv -> IO (action -> IO ())
startBotAsync bot env = do
  botEnv <- defaultBotEnv bot
  jobThreadIds <- scheduleBotJobs botEnv env (botJobs bot)
  fork_ $ startBotPolling bot botEnv
  return undefined
  where
    fork_ = void . forkIO . void . flip runClientM env

startBotAsync_ :: BotApp model action -> ClientEnv -> IO ()
startBotAsync_ bot env = void (startBotAsync bot env)

startBot :: BotApp model action -> ClientEnv -> IO (Either ServantError ())
startBot bot env = do
  botEnv <- defaultBotEnv bot
  jobThreadIds <- scheduleBotJobs botEnv env (botJobs bot)
  _actionsThreadId <- processActionsIndefinitely bot botEnv env
  runClientM (startBotPolling bot botEnv) env
startBot_ :: BotApp model action -> ClientEnv -> IO ()
startBot_ bot = void . startBot bot

issueAction :: BotEnv model action -> Maybe Telegram.Update -> action -> IO ()
issueAction BotEnv{..} update action = atomically $
  writeTQueue botActionsQueue (update, action)

processAction
  :: BotApp model action
  -> BotEnv model action
  -> Maybe Telegram.Update
  -> action
  -> ClientM ()
processAction BotApp{..} botEnv@BotEnv{..} update action = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (botHandler action model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  mapM_ ((>>= liftIO . issueAction botEnv update) . runBotM update) effects

processActionJob :: BotApp model action -> BotEnv model action -> ClientM ()
processActionJob botApp botEnv@BotEnv{..} = do
  (update, action) <- liftIO . atomically $ readTQueue botActionsQueue
  processAction botApp botEnv update action

processActionsIndefinitely
  :: BotApp model action -> BotEnv model action -> ClientEnv -> IO ThreadId
processActionsIndefinitely botApp botEnv env = forkIO . forever $ do
  runClientM (processActionJob botApp botEnv) env

startBotPolling :: BotApp model action -> BotEnv model action -> ClientM ()
startBotPolling BotApp{..} botEnv@BotEnv{..} = startPolling handleUpdate
  where
    handleUpdate update = liftIO . void . forkIO $ do
      maction <- botAction update <$> readTVarIO botModelVar
      case maction of
        Nothing     -> return ()
        Just action -> issueAction botEnv (Just update) action

startPolling :: (Telegram.Update -> ClientM ()) -> ClientM ()
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Telegram.UpdateId n) = Telegram.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        (Right <$> Telegram.getUpdates
          (Telegram.GetUpdatesRequest offset Nothing Nothing Nothing))
        `catchError` (pure . Left)

      nextUpdateId <- case res of
        Left servantErr -> do
          liftIO (print servantErr)
          pure lastUpdateId
        Right result -> do
          let updates = Telegram.responseResult result
              updateIds = map Telegram.updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay 1000000
      go nextUpdateId

-- | Get a 'Telegram.Token' from environment variable.
--
-- Common use:
--
-- @
-- 'getEnvToken' "TELEGRAM_BOT_TOKEN"
-- @
getEnvToken :: String -> IO Telegram.Token
getEnvToken varName = fromString <$> getEnv varName
