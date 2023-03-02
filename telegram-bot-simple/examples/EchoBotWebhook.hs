{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe
import           Data.Text                                       (Text)
import qualified Data.Text                                       as Text

import           Telegram.Bot.API
import           Telegram.Bot.API.InlineMode.InlineQueryResult
import           Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import           Telegram.Bot.API.Webhook
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser                (updateMessageSticker,
                                                                  updateMessageText)

import           Network.Wai.Handler.Warp                        (Port,
                                                                  defaultSettings,
                                                                  setPort)
import           Network.Wai.Handler.WarpTLS                     (OnInsecure (AllowInsecure),
                                                                  TLSSettings (onInsecure),
                                                                  tlsSettings)

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
        answerInlineQueryRequest = defAnswerInlineQuery queryId [result]
    _ <- liftClientM (answerInlineQuery answerInlineQueryRequest)
    return ()
  StickerEcho file chat -> model <# do
    _ <- liftClientM
      (sendSticker
        (defSendSticker (SomeChatId chat) file))
    return ()
  Echo msg -> model <# do
    pure msg -- or replyText msg

run :: Token -> FilePath -> FilePath -> Port -> String -> IO ()
run token certPath keyPath port ip = do
  env <- defaultTelegramClientEnv token
  res <- startBotWebhook bot config env
  print res
  where
    bot = conversationBot updateChatId echoBot
    tlsOpts = (tlsSettings certPath keyPath) {onInsecure = AllowInsecure}
    warpOpts = setPort port defaultSettings
    certFile = Just $ InputFile certPath "application/x-pem-file"
    url = "https://" ++ ip ++ ":" ++ show port
    config = WebhookConfig
               { webhookConfigTlsSettings       = tlsOpts,
                 webhookConfigTlsWarpSettings   = warpOpts,
                 webhookConfigSetWebhookRequest = requestData
               }
    requestData = (defSetWebhook url)
      { setWebhookCertificate    = certFile,
        setWebhookAllowedUpdates = Just ["message"]
      }

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
