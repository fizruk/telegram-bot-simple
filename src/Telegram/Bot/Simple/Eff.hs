{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Telegram.Bot.Simple.Eff where

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Data.Bifunctor
import           Servant.Client

import           Telegram.Bot.API

-- | Bot handler context.
--
-- The context may include an 'Update' the bot is handling at the moment.
newtype BotM a = BotM { _runBotM :: ReaderT (Maybe Update) ClientM a }
  deriving (Functor, Applicative, Monad, MonadReader (Maybe Update), MonadIO)

liftClientM :: ClientM a -> BotM a
liftClientM = BotM . lift

runBotM :: Maybe Update -> BotM a -> ClientM a
runBotM update = flip runReaderT update . _runBotM

newtype Eff action model = Eff { _runEff :: Writer [BotM action] model }
  deriving (Functor, Applicative, Monad)

instance Bifunctor Eff where
  bimap f g = Eff . mapWriter (bimap g (map (fmap f))) . _runEff

runEff :: Eff action model -> (model, [BotM action])
runEff = runWriter . _runEff

eff :: BotM a -> Eff a ()
eff e = Eff (tell [e])

(<#) :: model -> BotM action -> Eff action model
m <# a = eff a >> pure m

