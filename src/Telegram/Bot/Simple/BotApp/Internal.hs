{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Telegram.Bot.Simple.BotApp.Internal where

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad           (forever, void)
import           Control.Monad.Except    (catchError)
import           Control.Monad.Trans     (liftIO)
import           Data.Bifunctor          (first)
import           Data.Text               (Text)
import           Servant.Client          (ClientEnv, ClientM, runClientM)
import qualified System.Cron             as Cron

import qualified Telegram.Bot.API        as Telegram
import           Telegram.Bot.Simple.Eff

-- | A bot application.
data BotApp model action = BotApp
  { botInitialModel :: model
    -- ^ Initial bot state.
  , botAction       :: Telegram.Update -> model -> Maybe action
    -- ^ How to convert incoming 'Telegram.Update's into @action@s.
    -- See "Telegram.Bot.Simple.UpdateParser" for some helpers.
  , botHandler      :: action -> model -> Eff action model
    -- ^ How to handle @action@s.
  , botJobs         :: [BotJob model action]
    -- ^ Background bot jobs.
  }

-- | A background bot job.
data BotJob model action = BotJob
  { botJobSchedule :: Text
    -- ^ Cron schedule for the job.
  , botJobTask     :: model -> Eff action model
    -- ^ Job function.
  }

-- | An environment actual bot runs in.
data BotEnv model action = BotEnv
  { botModelVar     :: TVar model
    -- ^ A transactional variable with bot's current state.
  , botActionsQueue :: TQueue (Maybe Telegram.Update, action)
    -- ^ A queue of @action@s to process (with associated 'Telegram.Update's).
  , botClientEnv    :: ClientEnv
    -- ^ HTTP client environment (where and how exactly to make requests to Telegram Bot API).
    -- This includes 'Telegram.Token'.
  , botUser         :: Telegram.User
    -- ^ Information about the bot in the form of 'Telegram.User'.
  }

instance Functor (BotJob model) where
  fmap f BotJob{..} = BotJob{ botJobTask = first f . botJobTask, .. }

-- | Run bot job task once.
runJobTask :: BotEnv model action -> (model -> Eff action model) -> IO ()
runJobTask botEnv@BotEnv{..} task = do
  effects <- liftIO $ atomically $ do
    model <- readTVar botModelVar
    case runEff (task model) of
      (newModel, effects) -> do
        writeTVar botModelVar newModel
        return effects
  res <- flip runClientM botClientEnv $
    mapM_ ((>>= liftIO . issueAction botEnv Nothing) . runBotM (BotContext botUser Nothing)) effects
  case res of
    Left err -> print err
    Right _  -> return ()

-- | Schedule a cron-like bot job.
scheduleBotJob :: BotEnv model action -> BotJob model action -> IO [ThreadId]
scheduleBotJob botEnv BotJob{..} = Cron.execSchedule $ do
  Cron.addJob (runJobTask botEnv botJobTask) botJobSchedule

-- | Schedule all bot jobs.
scheduleBotJobs :: BotEnv model action -> [BotJob model action] -> IO [ThreadId]
scheduleBotJobs botEnv jobs = concat
  <$> traverse (scheduleBotJob botEnv) jobs

-- | Construct a default @'BotEnv' model action@ for a bot.
defaultBotEnv :: BotApp model action -> ClientEnv -> IO (BotEnv model action)
defaultBotEnv BotApp{..} env = BotEnv
  <$> newTVarIO botInitialModel
  <*> newTQueueIO
  <*> pure env
  <*> (either (error . show) Telegram.responseResult <$> runClientM Telegram.getMe env)

-- | Issue a new action for the bot to process.
issueAction :: BotEnv model action -> Maybe Telegram.Update -> action -> IO ()
issueAction BotEnv{..} update action = atomically $
  writeTQueue botActionsQueue (update, action)

-- | Process one action.
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
  mapM_ ((>>= liftIO . issueAction botEnv update) . runBotM (BotContext botUser update)) effects

-- | A job to wait for the next action and process it.
processActionJob :: BotApp model action -> BotEnv model action -> ClientM ()
processActionJob botApp botEnv@BotEnv{..} = do
  (update, action) <- liftIO . atomically $ readTQueue botActionsQueue
  processAction botApp botEnv update action

-- | Process incoming actions indefinitely.
processActionsIndefinitely
  :: BotApp model action -> BotEnv model action -> IO ThreadId
processActionsIndefinitely botApp botEnv = forkIO . forever $ do
  runClientM (processActionJob botApp botEnv) (botClientEnv botEnv)

-- | Start 'Telegram.Update' polling for a bot.
startBotPolling :: BotApp model action -> BotEnv model action -> ClientM ()
startBotPolling BotApp{..} botEnv@BotEnv{..} = startPolling handleUpdate
  where
    handleUpdate update = liftIO . void . forkIO $ do
      maction <- botAction update <$> readTVarIO botModelVar
      case maction of
        Nothing     -> return ()
        Just action -> issueAction botEnv (Just update) action

-- | Start 'Telegram.Update' polling with a given update handler.
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
