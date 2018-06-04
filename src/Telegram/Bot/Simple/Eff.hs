{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Telegram.Bot.Simple.Eff where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Bifunctor
import           Servant.Client

import qualified Telegram.Bot.API     as Telegram

-- | Bot handler context.
--
-- The context may include an 'Update' the bot is handling at the moment.
newtype BotM a = BotM { _runBotM :: ReaderT BotContext ClientM a }
  deriving (Functor, Applicative, Monad, MonadReader BotContext, MonadIO)

data BotContext = BotContext
  { botContextUser   :: Telegram.User
  , botContextUpdate :: Maybe Telegram.Update
  }

liftClientM :: ClientM a -> BotM a
liftClientM = BotM . lift

runBotM :: BotContext -> BotM a -> ClientM a
runBotM update = flip runReaderT update . _runBotM

newtype Eff action model = Eff { _runEff :: Writer [BotM action] model }
  deriving (Functor, Applicative, Monad)

instance Bifunctor Eff where
  bimap f g = Eff . mapWriter (bimap g (map (fmap f))) . _runEff

runEff :: Eff action model -> (model, [BotM action])
runEff = runWriter . _runEff

eff :: BotM a -> Eff a ()
eff e = Eff (tell [e])

withEffect :: BotM action -> model -> Eff action model
withEffect effect model = eff effect >> pure model

(<#) :: model -> BotM action -> Eff action model
(<#) = flip withEffect

-- | Set a specific 'Telegram.Update' in a 'BotM' context.
setBotMUpdate :: Maybe Telegram.Update -> BotM a -> BotM a
setBotMUpdate update (BotM m) = BotM (local f m)
  where
    f botContext = botContext { botContextUpdate = update }

-- | Set a specific 'Telegram.Update' in every effect of 'Eff' context.
setEffUpdate :: Maybe Telegram.Update -> Eff action model -> Eff action model
setEffUpdate update (Eff m) = Eff (censor (map (setBotMUpdate update)) m)
