{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Bot.Simple where

import Control.Monad.Reader
import Control.Monad.Writer
import qualified Data.Text as Text
import Data.String (IsString(..))
import Servant.Client

import Telegram.Bot.API
import Telegram.Bot.Handler

data SampleState
  = Choosing
  | TypingReply
  | TypingChoice

type BotHandler s = ReaderT s (ReaderT Update ClientM)
type BotHandlers u s = Handlers (BotHandler s) u s

data BotConfig s = BotConfig
  { botHandlers :: BotHandlers Update s
  }

instance Monoid (BotConfig s) where
  mempty = BotConfig mempty
  BotConfig h1 `mappend` BotConfig h2 = BotConfig (h1 `mappend` h2)

newtype BotBuilder s a = BotBuilder { runBotBuilder :: Writer (BotConfig s) a }
  deriving (Functor, Applicative, Monad, MonadWriter (BotConfig s))

handle
  :: (BotHandlers t s -> BotHandlers Update s)
  -> (t -> BotHandler s s)
  -> BotBuilder s ()
handle s h = tell (BotConfig (s (handler h)))


bot :: BotConfig ()
bot = execWriter . runBotBuilder $ do
  handle (command "start") $ \Command{..} -> do
    reply
      "Hi! My name is Doctor Botter. \
      \I will hold a more complex conversation with you. \
      \Why don't you tell me something about yourself?"
      { replyMessageReplyMarkup = Just markup }

  handle textMessage $ \text -> do
    if (text `elem` ["Age", "Favourite colour", "Number of siblings"])
      then reply (fromString ("Your " <> Text.unpack text <> "? Yes, I would love to hear about that!"))
      else reply "Alright, please send me the category first, for example \"Most impressive skill\""
  where
    markup = SomeReplyKeyboardMarkup $ ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard = buttons
      , replyKeyboardMarkupResizeKeyboard = Nothing
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      }
    buttons =
      [ [ "Age",                "Favourite colour" ]
      , [ "Number of siblings", "Something else..." ]
      , [ "Done" ] ]

simpleBot :: (Message -> ReplyMessage) -> ClientM ()
simpleBot f = startPolling (hoistHandlers (flip runReaderT ()) botHandlers)
  where
    BotConfig{..} = execWriter . runBotBuilder $ do
      handle message $ \msg -> do
        reply (f msg)
        return ()



data BotApp m model action = BotApp
  { botAppInitialModel :: model
  , botAppActions      :: Update -> model -> Maybe action
  , botAppHandler      :: action -> model -> m model
  , botAppJobs         :: [BotJob m model action]
  }

data BotJob m model action = BotJob
  { botJobSchedule :: Int
  , botJobTask     :: model -> m model
  }

