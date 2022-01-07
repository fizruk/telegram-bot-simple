{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleInstances          #-}
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

newtype Eff action model = Eff { _runEff :: Writer [Bot action] model }
  deriving (Functor, Applicative, Monad)


data Bot action where
  ExistBot :: (BotContext -> BotM a -> ClientM (Maybe action)) -> BotM a -> Bot action

applyBot :: BotContext -> Bot action -> ClientM (Maybe action)
applyBot context (ExistBot run effect) = run context effect
instance Functor Bot where
   fmap f (ExistBot run effect) = ExistBot ((fmap . fmap . fmap . fmap) f run) effect

instance Applicative Bot where
  pure a = ExistBot (pure . pure . pure . pure $ a) (pure ())
  (ExistBot runF effF) <*> (ExistBot runV effV) = ExistBot newRun tupleEff
    where
      tupleEff = (,) <$> effF <*> effV
      newRun context effect = do
        val <- runV context (snd <$> effect)
        func <- runF context (fst <$> effect)
        pure (func <*> val)

instance Monad Bot where
  (ExistBot run effect) >>= fun = ExistBot newRun effect
    where
      newRun context effect = do
        x <- run context effect
        case x of
          Nothing -> pure Nothing
          Just a -> do
            applyBot context (fun a) 

instance MonadIO Bot where
  liftIO action = ExistBot ((const . const) $ Just <$> liftIO action) (pure ())
class RunBot a action where
  runBot :: BotContext -> BotM a -> ClientM (Maybe action)

instance Bifunctor Eff where
  bimap f g = Eff . mapWriter (bimap g (map (fmap f))) . _runEff

runEff :: Eff action model -> (model, [Bot action])
runEff = runWriter . _runEff

eff :: RunBot a b => BotM a -> Eff b ()
eff e = Eff (tell [ExistBot runBot e])

withEffect :: RunBot a action => BotM a -> model -> Eff action model
withEffect effect model = eff effect >> pure model

(<#) :: RunBot a action => model -> BotM a -> Eff action model
(<#) = flip withEffect

-- | Set a specific 'Telegram.Update' in a 'BotM' context.
setBotMUpdate :: Maybe Telegram.Update -> Bot a -> Bot a
setBotMUpdate update (ExistBot run (BotM m)) = ExistBot run $ BotM (local f m)
  where
    f botContext = botContext { botContextUpdate = update }

-- | Set a specific 'Telegram.Update' in every effect of 'Eff' context.
setEffUpdate :: Maybe Telegram.Update -> Eff action model -> Eff action model
setEffUpdate update (Eff m) = Eff (censor (map (setBotMUpdate update)) m)
