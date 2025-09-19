{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Telegram.Bot.Simple.BotApp.Internal where

import Control.Concurrent (ThreadId, threadDelay)
import Control.Concurrent.Async (Async, async, asyncThreadId, forConcurrently_, link)
import Control.Concurrent.STM
import Control.Exception (fromException)
import Control.Monad (forever, forM, void, when, (<=<))
import Control.Monad.Except (catchError)
import Control.Monad.Trans (liftIO)
import Data.Aeson.Types (parseEither, parseJSON)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Either (partitionEithers)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import Servant.Client (ClientEnv, ClientError(..), ClientM, runClientM)
import System.Cron qualified as Cron

import Telegram.Bot.API qualified as Telegram
import Telegram.Bot.Simple.Eff

-- | A policy on how to act on retries.
data RetryPolicy action = RetryPolicy
  { retryPolicyRetries :: HashMap Int (TBQueue (Maybe Telegram.Update, action))
    -- ^ Amount of retries. 'Nothing' means there will be no retries.

  , retryPolicyWaitSeconds :: Telegram.Seconds
    -- ^ Waiting interval between retries.
  }

-- | No retries will be performed.
noRetryPolicy :: RetryPolicy action
noRetryPolicy = RetryPolicy HashMap.empty 0

-- | Default policy: 10 retries, 1 second interval between retries.
defaultRetryPolicy :: IO (RetryPolicy action)
defaultRetryPolicy = do
  retryPolicyRetries <- HashMap.fromList <$> forM [1..10] \key -> do
    queue <- newTBQueueIO 20_000
    pure (key, queue)
  let retryPolicyWaitSeconds = 1
  pure RetryPolicy {..}

processRetries
  :: RetryPolicy action
  -> BotApp model action
  -> BotEnv model action
  -> (Int, TBQueue (Maybe Telegram.Update, action))
  -> IO ()
processRetries RetryPolicy{..} botApp botEnv@BotEnv{..} (key, rqueue) =
  (liftIO . atomically . tryReadTBQueue) rqueue >>= \case
    Nothing -> pure ()
    Just (update, action) -> runClientM (processAction botApp botEnv update action) botClientEnv >>= \case
      Left err -> case err of
        ConnectionError exc -> case fromException exc of
          -- Underlying http-client fails to establish connection
          Just (HttpExceptionRequest _req ConnectionTimeout) -> do
            let retries = pred key
            print $ concat ["connection timeout. ", show retries, " retries left."]
            case HashMap.lookup retries retryPolicyRetries of
              Nothing -> do
                print $ concat
                  [ "retries exhausted or queue not found: ", show retries, " left. Request:"]
                print ("processRetries" :: String, retries, err)
              Just queue -> liftIO $ atomically $ writeTBQueue queue (update, action)
          _ -> print ("processRetries" :: String, err)
        _ -> print ("processRetries" :: String, err)
      Right _ -> pure ()

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
  , botActionsQueue :: TBQueue (Maybe Telegram.Update, action)
    -- ^ A queue of @action@s to process (with associated 'Telegram.Update's).
  , botClientEnv    :: ClientEnv
    -- ^ HTTP client environment (where and how exactly to make requests to Telegram Bot API).
    -- This includes 'Telegram.Token'.
  , botUser         :: Telegram.User
    -- ^ Information about the bot in the form of 'Telegram.User'.
  , botRetryPolicy  :: RetryPolicy action
    -- ^ Retry policy on actions involving 'ClientM' that have failed to fire.
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
    mapM_ ((liftIO . issueAction botEnv Nothing) <=< runBotM (BotContext botUser Nothing)) effects
  case res of
    Left err -> print ("runJobTask" :: String, err)
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
  <*> newTBQueueIO 20_000
  <*> pure env
  <*> (either (error . show) Telegram.responseResult <$> runClientM Telegram.getMe env)
  <*> defaultRetryPolicy

-- | Issue a new action for the bot to process.
issueAction :: BotEnv model action -> Maybe Telegram.Update -> Maybe action -> IO ()
issueAction BotEnv{..} update (Just action) = atomically $
  writeTBQueue botActionsQueue (update, action)
issueAction _ _ _ = pure ()

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
  let runBotAndIssueAction
        = (liftIO . issueAction botEnv update) <=< runBotM (BotContext botUser update)
  mapM_ runBotAndIssueAction effects

-- | A job to wait for the next action and process it.
processActionJob :: BotApp model action -> BotEnv model action -> ClientM ()
processActionJob botApp botEnv@BotEnv{..} = do
  (update, action) <- liftIO . atomically $ readTBQueue botActionsQueue
  processAction botApp botEnv update action

-- | Process incoming actions indefinitely.
processActionsIndefinitely
  :: BotApp model action -> BotEnv model action -> IO ThreadId
processActionsIndefinitely botApp botEnv@BotEnv{..} = do
  a <- asyncLink $ do
    let retryQueues = zip (repeat True) $ HashMap.toList $ retryPolicyRetries botRetryPolicy
        fallback = (False, (succ $ HashMap.size (retryPolicyRetries botRetryPolicy), botActionsQueue))
    forConcurrently_ (fallback : retryQueues) \(itShouldWait, retryData) -> forever do
      processRetries botRetryPolicy botApp botEnv retryData
      when itShouldWait
        $ threadDelay (coerce (retryPolicyWaitSeconds botRetryPolicy) * 1_000_000)
  return (asyncThreadId a)

-- | Start 'Telegram.Update' polling for a bot.
startBotPolling :: BotApp model action -> BotEnv model action -> ClientM ()
startBotPolling BotApp{..} botEnv@BotEnv{..} = startPolling handleUpdate
  where
    handleUpdate update = liftIO . void . asyncLink $ do
      maction <- botAction update <$> readTVarIO botModelVar
      case maction of
        Nothing     -> return ()
        Just action -> issueAction botEnv (Just update) (Just action)

-- | Start 'Telegram.Update' polling with a given update handler.
startPolling :: (Telegram.Update -> ClientM a) -> ClientM a
startPolling handleUpdate = go Nothing
  where
    go lastUpdateId = do
      let inc (Telegram.UpdateId n) = Telegram.UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <-
        (Right <$> Telegram.getUpdatesAsValue
          (Telegram.GetUpdatesRequest offset Nothing (Just 25) Nothing))
        `catchError` (pure . Left)

      nextUpdateId <- case res of
        Left servantErr -> do
          liftIO $ print ("startPolling.go" :: String, servantErr)
          pure lastUpdateId
        Right result -> do
          let updateValues = Telegram.responseResult result
              (errors, updates) = parseUpdates updateValues
              updateIds = map Telegram.updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ reportParseError errors
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay 1000000
      go nextUpdateId

    parseUpdates updates =
      partitionEithers (map (parseEither parseJSON) updates)

    reportParseError err =
      liftIO $ putStrLn $
        "Failed to parse an update! Please, make sure you have the latest version of `telegram-bot-api`\
        \ library and consider opening an issue if so. Error message: " <> err

-- ** Helpers

-- | Instead of 'forkIO' which hides exceptions,
-- allow users to handle those exceptions separately.
--
-- See <https://github.com/fizruk/telegram-bot-simple/issues/159>.
asyncLink :: IO a -> IO (Async a)
asyncLink action = do
  a <- async action
  link a
  return a
