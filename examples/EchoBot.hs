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

type Model = ()

data Action
  = NoOp
  | InlineEcho InlineQueryId Text
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
  NoOp -> pure model
  InlineEcho queryId msg -> model <# do
    _ <- liftClientM (
      answerInlineQuery (
          AnswerInlineQueryRequest
            queryId
            [
              InlineQueryResult InlineQueryResultArticle (InlineQueryResultId msg) (Just msg) (Just (defaultInputTextMessageContent msg))
            ]
        )
      )
    return NoOp
  StickerEcho file chat -> model <# do
    _ <- liftClientM 
      (sendSticker 
        (SendStickerRequest 
          (SomeChatId chat) 
          file 
          Nothing 
          Nothing 
          Nothing 
          Nothing))
    return NoOp
  Echo msg -> model <# do
    replyText msg
    return NoOp

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ echoBot env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
