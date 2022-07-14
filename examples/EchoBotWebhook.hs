-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Network.Wai.Handler.WarpTLS
-- import Servant
-- import Servant.API.Generic (Generic)
-- import Data.Aeson (ToJSON, FromJSON)
-- import Data.List (intercalate)

-- type API = ReqBody '[JSON] () :> Get '[JSON] Email


-- data ClientInfo = ClientInfo
--   { 
--   } deriving Generic

-- instance FromJSON ClientInfo
-- instance ToJSON ClientInfo

-- data Email = Email
--   { from :: String
--   , to :: String
--   , subject :: String
--   , body :: String
--   } deriving Generic

-- instance ToJSON Email

-- emailForClient :: () -> Email
-- emailForClient c = Email from' to' subject' body'

--   where from'    = "great@company.com"
--         to'      = "xD"
--         subject' = "Hey " ++ "ja" ++ ", we miss you!"
--         body'    = "Hi " ++ "ja" ++ ",\n\n"
--                 ++ "Since you've recently turned " ++ show 2137
--                 ++ ", have you checked out our latest "
--                 ++ intercalate ", " []
--                 ++ " products? Give us a visit!"

-- server3 :: Server API
-- server3 = marketing

--   where 
--         marketing :: () -> Handler Email
--         marketing clientinfo = return (emailForClient clientinfo)

-- userAPI :: Proxy API
-- userAPI = Proxy

-- main :: IO ()
-- main = runTLS tlsOpts warpOpts $ serve userAPI server3
--   where tlsOpts = tlsSettings "../cert.pem" "../key.pem"
--         warpOpts = setPort 8443 defaultSettings

{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Maybe

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText, updateMessageSticker)
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)

import           Network.Wai.Handler.Warp (setPort, setHost, defaultSettings, Port)
import           Network.Wai.Handler.WarpTLS (tlsSettings, TLSSettings (onInsecure), OnInsecure (AllowInsecure))


type Model = ()

data Action
  = InlineEcho InlineQueryId Text
  | StickerEcho InputFile ChatId
  | Echo Text

echoBot :: BotApp Model Action
echoBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateInlineQuery update =  do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
      let msg =  inlineQueryQuery query
      Just $ InlineEcho queryId msg
  | isJust $ updateMessageSticker update = do
    fileId <- stickerFileId <$> updateMessageSticker update
    chatId <- updateChatId update
    pure $ StickerEcho (InputFileId fileId) chatId
  | otherwise = case updateMessageText update of
      Just text -> Just (Echo text)
      Nothing   -> Nothing

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  InlineEcho queryId msg -> model <# do
    let result = InlineQueryResult InlineQueryResultArticle (InlineQueryResultId msg) (Just msg) (Just (defaultInputTextMessageContent msg)) Nothing
        answerInlineQueryRequest = AnswerInlineQueryRequest
          { answerInlineQueryRequestInlineQueryId = queryId
          , answerInlineQueryRequestResults       = [result]
          , answerInlineQueryCacheTime            = Nothing
          , answerInlineQueryIsPersonal           = Nothing
          , answerInlineQueryNextOffset           = Nothing
          , answerInlineQuerySwitchPmText         = Nothing
          , answerInlineQuerySwitchPmParameter    = Nothing
          }
    _ <- liftClientM (answerInlineQuery answerInlineQueryRequest)
    return ()
  StickerEcho file chat -> model <# do
    _ <- liftClientM 
      (sendSticker 
        (SendStickerRequest 
          (SomeChatId chat) 
          file 
          Nothing
          Nothing
          Nothing 
          Nothing 
          Nothing))
    return ()
  Echo msg -> model <# do
    pure msg -- or replyText msg

run :: Token -> FilePath -> FilePath -> Port -> String -> IO ()
run token certPath keyPath port ip = do
  env <- defaultTelegramClientEnv token
  res <- startBotWebHooks bot tlsOpts warpOpts certFile ip env
  print res
  where 
    bot = conversationBot updateChatId echoBot
    tlsOpts = (tlsSettings certPath keyPath) {onInsecure = AllowInsecure}
    warpOpts = setPort port $ setHost "0.0.0.0" defaultSettings
    certFile = Just $ InputFile certPath "application/x-pem-file"

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  putStrLn "Please, enter a path to certificate:"
  cert <- getLine
  putStrLn "Please, enter a path to key:"
  key <- getLine
  putStrLn "Please, enter port(80, 88, 443, 8443):"
  port <- read <$> getLine
  putStrLn "Please, enter ip:"
  ip <- getLine
  run token cert key port ip
