{-# LANGUAGE BangPatterns #-}
module Telegram.Bot.Simple.Debug where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Writer       (tell)
import           Data.Aeson                 (ToJSON)
import qualified Data.Aeson.Encode.Pretty   as Aeson
import           Data.Monoid                ((<>))
import qualified Data.Text.Lazy             as Text
import qualified Data.Text.Lazy.Encoding    as Text
import           Debug.Trace                (trace)
import           Text.Show.Pretty           (ppShow)

import qualified Telegram.Bot.API           as Telegram
import           Telegram.Bot.Simple.BotApp
import           Telegram.Bot.Simple.Eff

-- * Bot debug tracing

-- | This a default bot tracing modifier that relies on
--
-- * 'traceTelegramUpdatesJSON'
-- * 'traceBotActionsShow'
-- * 'traceBotModelShow'
traceBotDefault
  :: (Show model, Show action)
  => BotApp model action
  -> BotApp model action
traceBotDefault
  = traceTelegramUpdatesJSON
  . traceBotActionsShow
  . traceBotModelShow

-- ** Trace 'Telegram.Update's

-- | Trace (debug print) every 'Telegram.Update' before parsing it.
traceTelegramUpdatesWith
  :: (Telegram.Update -> String)    -- ^ How to display an update.
  -> BotApp model action
  -> BotApp model action
traceTelegramUpdatesWith f botApp = botApp
  { botAction = \update -> botAction botApp $! trace (f update) update
  }

-- | Trace (debug print) every update as pretty JSON value.
traceTelegramUpdatesJSON :: BotApp model action -> BotApp model action
traceTelegramUpdatesJSON = traceTelegramUpdatesWith ppAsJSON

-- | Trace (debug print) every update using 'Show' instance.
traceTelegramUpdatesShow :: BotApp model action -> BotApp model action
traceTelegramUpdatesShow = traceTelegramUpdatesWith ppShow

-- ** Trace bot actions

-- | A type of an action to trace.
data TracedAction action
  = TracedIncomingAction action  -- ^ An action that's about to be handled.
  | TracedIssuedAction action    -- ^ An action that's just been issued by some handler.
  deriving (Eq, Show)

-- | Pretty print 'TraceActionType'.
ppTracedAction :: Show action => TracedAction action -> String
ppTracedAction (TracedIncomingAction action) = "Incoming: " <> ppShow action
ppTracedAction (TracedIssuedAction   action) = "Issued:   " <> ppShow action

-- | Trace (debug print) every incoming and issued action.
traceBotActionsWith
  :: (TracedAction action -> String)  -- ^ How to display an action.
  -> BotApp model action
  -> BotApp model action
traceBotActionsWith f botApp = botApp { botHandler = newHandler }
  where
    traceAction action = action <$ do
      liftIO $ putStrLn (f (TracedIssuedAction action))

    newHandler !action model = do
      Eff (tell (map (>>= traceAction) actions))
      pure newModel
      where
        (newModel, actions) = runEff $
          botHandler botApp
            (trace (f (TracedIncomingAction action)) action)
            model

-- | Trace (debug print) bot actions using 'Show' instance.
traceBotActionsShow
  :: Show action => BotApp model action -> BotApp model action
traceBotActionsShow = traceBotActionsWith ppTracedAction

-- ** Trace bot state model

-- | Trace (debug print) bot model.
traceBotModelWith
  :: (model -> String)    -- ^ How to display a model.
  -> BotApp model action
  -> BotApp model action
traceBotModelWith f botApp = botApp
  { botInitialModel = newInitialModel
  , botHandler = newHandler
  }
    where
      !newInitialModel = traceModel (botInitialModel botApp)
      newHandler action !model = traceModel <$> botHandler botApp action model
      traceModel = trace <$> f <*> id

-- | Trace (debug print) bot model using 'Show' instance.
traceBotModelShow
  :: Show model => BotApp model action -> BotApp model action
traceBotModelShow = traceBotModelWith ppShow

-- | Trace (debug print) bot model using 'Show' instance.
traceBotModelJSON
  :: ToJSON model => BotApp model action -> BotApp model action
traceBotModelJSON = traceBotModelWith ppAsJSON

-- * Helpers

-- | Pretty print a value as JSON.
ppAsJSON :: ToJSON a => a -> String
ppAsJSON = Text.unpack . Text.decodeUtf8 . Aeson.encodePretty
