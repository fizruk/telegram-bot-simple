module Telegram.Bot.Simple.Debug where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Writer       (tell)
import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Monoid                ((<>))
import           Debug.Trace                (trace)

import qualified Telegram.Bot.API           as Telegram
import           Telegram.Bot.Simple.BotApp
import           Telegram.Bot.Simple.Eff

-- * Trace 'Telegram.Update's

-- | Trace (debug print) every 'Telegram.Update' before parsing it.
traceTelegramUpdatesWith
  :: (Telegram.Update -> String)    -- ^ How to display an update.
  -> BotApp model action
  -> BotApp model action
traceTelegramUpdatesWith f botApp = botApp
  { botAction = \update -> botAction botApp (trace (f update) update)
  }

-- | Trace (debug print) every update as pretty JSON value.
traceTelegramUpdatesJSON :: BotApp model action -> BotApp model action
traceTelegramUpdatesJSON = traceTelegramUpdatesWith prettyJSONString

-- | Trace (debug print) every update using 'Show' instance.
traceTelegramUpdatesShow :: BotApp model action -> BotApp model action
traceTelegramUpdatesShow = traceTelegramUpdatesWith show

-- * Trace bot actions

-- | Trace (debug print) every incoming and issued action.
traceBotActionsWith
  :: (action -> String)     -- ^ How to display an action.
  -> BotApp model action
  -> BotApp model action
traceBotActionsWith f botApp = botApp { botHandler = newBotHandler }
  where
    traceAction action = action <$ do
      liftIO $ putStrLn ("Issued action: " <> f action)

    newBotHandler action model = do
      Eff (tell (map (>>= traceAction) actions))
      pure newModel
      where
        (newModel, actions) = runEff $
          botHandler botApp
            (trace ("Incoming action: " <> f action) action)
            model

-- | Trace (debug print) bot actions using 'Show' instance.
traceBotActionsShow
  :: Show action => BotApp model action -> BotApp model action
traceBotActionsShow = traceBotActionsWith show

-- * Trace bot state model

-- | Trace (debug print) bot model.
traceBotModelWith
  :: (model -> String)    -- ^ How to display a model.
  -> BotApp model action
  -> BotApp model action
traceBotModelWith f botApp = botApp
  { botInitialModel = traceModel (botInitialModel botApp)
  , botHandler = newHandler
  }
    where
      traceModel = trace <$> f <*> id

      newHandler action model = traceModel <$> botHandler botApp action model

-- | Trace (debug print) bot model using 'Show' instance.
traceBotModelShow
  :: Show model => BotApp model action -> BotApp model action
traceBotModelShow = traceBotModelWith show

-- | Trace (debug print) bot model using 'Show' instance.
traceBotModelJSON
  :: ToJSON model => BotApp model action -> BotApp model action
traceBotModelJSON = traceBotModelWith prettyJSONString

-- * Helpers

prettyJSONString :: ToJSON a => a -> String
prettyJSONString = BSL8.unpack . Aeson.encodePretty
