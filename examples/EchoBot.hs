{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           Telegram.Bot.API
import           Telegram.Bot.Simple
import           Telegram.Bot.Simple.UpdateParser (updateMessageText)

type Model = ()

data Action
  = NoOp
  | Echo Text
  deriving(Show)

echoBot :: BotApp (Maybe ChatId, Model) Action
echoBot = BotApp
  { botInitialModel = (Nothing, ())
  , botAction = updateToAction
  , botHandler = handleAction
  , botJobs = []
  }

updateToAction :: Update -> (Maybe ChatId, Model) -> Maybe Action
updateToAction update _ =
  case updateMessageText update of
    Just text -> Just (Echo text)
    Nothing   -> Nothing

handleAction :: Action -> (Maybe ChatId, Model) -> Eff Action (Maybe ChatId, Model)
handleAction action model = case action of
  NoOp -> pure model
  Echo msg -> model <# do
    replyText msg
    return NoOp

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId echoBot) env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
