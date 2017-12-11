{-# LANGUAGE RecordWildCards #-}
module Telegram.Bot.Simple where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Text (Text)
import Servant.Client

import Telegram.Bot.API

type Bot = ClientM ()

simpleBot :: (Text -> Text) -> Bot
simpleBot reply = go Nothing
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

    handleUpdate Update{..} = do
      case updateMessage of
        Just Message{..} -> case messageChat of
          Chat{..} -> case messageText of
            Just msg -> do
              sendMessage $ SendMessageRequest
                (SomeChatId chatId)
                (reply msg)
                Nothing
                Nothing
                Nothing
                Nothing
                Nothing
              return ()
            _ -> return ()
        _ -> return ()
