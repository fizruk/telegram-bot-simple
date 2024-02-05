{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
module Telegram.Bot.Simple.Webhook (webhookApp) where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class              (MonadIO (liftIO))
import           Data.Functor                        (void)
import           Servant

import           Telegram.Bot.API.GettingUpdates     (Update)
import           Telegram.Bot.Simple.BotApp.Internal

type WebhookAPI = ReqBody '[JSON] Update :> Post '[JSON] ()

server :: BotApp model action -> BotEnv model action -> Server WebhookAPI
server BotApp {..} botEnv@BotEnv {..} =
  updateHandler
  where
    updateHandler :: Update -> Handler ()
    updateHandler update = liftIO $ handleUpdate update
    handleUpdate update = liftIO . void . asyncLink $ do
      maction <- botAction update <$> readTVarIO botModelVar
      case maction of
        Nothing     -> return ()
        Just action -> issueAction botEnv (Just update) (Just action)

webhookAPI :: Proxy WebhookAPI
webhookAPI = Proxy

app :: BotApp model action -> BotEnv model action -> Application
app botApp botEnv = serve webhookAPI $ server botApp botEnv

webhookApp :: BotApp model action -> BotEnv model action -> Application
webhookApp = app

