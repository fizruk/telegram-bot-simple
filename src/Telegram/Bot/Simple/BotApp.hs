{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Telegram.Bot.Simple.BotApp where

import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Exception.Lifted
import           Control.Monad.Except
import           Control.Monad.Trans.Control
import           Servant.Client

import           Telegram.Bot.API
import           Telegram.Bot.Simple.Eff

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
startBotAsync_ bot env = void (startBotAsync bot env)

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
      res <- try $
        (Right <$> getUpdates
          (GetUpdatesRequest offset Nothing Nothing Nothing))
        `catchError` (pure . Left)

      nextUpdateId <- case res of
        Left (ex :: SomeException) -> do
          liftIO (print ex)
          pure lastUpdateId
        Right (Left servantErr) -> do
          liftIO (print servantErr)
          pure lastUpdateId
        Right (Right result) -> do
          let updates = responseResult result
              updateIds = map updateUpdateId updates
              maxUpdateId = maximum (Nothing : map Just updateIds)
          mapM_ handleUpdate updates
          pure maxUpdateId
      liftIO $ threadDelay 1000000
      go nextUpdateId

