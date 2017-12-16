module Main where

import Telegram.Bot.Simple
import Servant.Client

type Model = ()
type Action = ()

echoBot :: BotApp ClientM Model Action
echoBot = BotApp
  { botAppInitialModel = ()
  , botAppActions = \_update () -> Nothing
  , botAppHandler = \() () -> pure ()
  , botAppJobs = []
  }

main :: IO ()
main = undefined
