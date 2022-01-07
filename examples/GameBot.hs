{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (isJust)
import Data.Text (Text)
import Options.Applicative hiding (action)

import qualified Data.Text as Text

import Telegram.Bot.API
import Telegram.Bot.API.InlineMode.InlineQueryResult
import Telegram.Bot.API.InlineMode.InputMessageContent (defaultInputTextMessageContent)
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser (updateMessageText)

type Model = ()

data Action
  = NoOp
  | AGame Text
  | AInlineGame InlineQueryId Text
  | AFeedback Text

gameBot :: BotApp Model Action
gameBot = BotApp
  { botInitialModel = ()
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> Model -> Maybe Action
updateToAction update _
  | isJust $ updateMessageText update = game
  | isJust $ updateInlineQuery update = do
      query <- updateInlineQuery update
      let queryId = inlineQueryId query
          msg     = inlineQueryQuery query
      Just $ AInlineGame queryId msg
  | otherwise = Nothing
  where
    game = Just $ AGame "http://localhost"

handleAction :: Action -> Model -> Eff Action Model
handleAction action model = case action of
  NoOp -> pure model
  AFeedback _text -> error "TBD"
  AInlineGame queryId msg -> model <# do
    _ <- liftClientM (
      answerInlineQuery (
          AnswerInlineQueryRequest
            queryId
            [
              InlineQueryResult InlineQueryResultGame (InlineQueryResultId msg) (Just msg) (Just (defaultInputTextMessageContent "http://localhost"))
            ]
        )
      )
    return NoOp
  AGame game -> model <# do
    replyText game
    return NoOp

data Command = CmdBot | CmdServer

-- * Main

main :: IO ()
main = execParser (info (commands <**> helper) idm) >>= \case
  CmdBot -> runBot
  CmdServer -> runServer

commands :: Parser Command
commands = subparser
    (command "bot"    (info botOpts    (progDesc botDesc)) <>
     command "server" (info serverOpts (progDesc serverDesc)))
  where
    botDesc = "Run Telegram Game Bot"
    serverDesc = "Run HTML5 Game Server"

    botOpts = pure CmdBot
    serverOpts = pure CmdServer

-- * Bot

runBot :: IO ()
runBot = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  env <- defaultTelegramClientEnv token
  startBot_ gameBot env

-- * Server

runServer :: IO ()
runServer = error "TBD"
