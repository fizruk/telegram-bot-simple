{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Telegram.Bot.Simple.UpdateParser where

import           Control.Applicative
import           Control.Monad.Reader
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Text.Read            (readMaybe)

import           Telegram.Bot.API

newtype UpdateParser a = UpdateParser
  { runUpdateParser :: Update -> Maybe a
  } deriving (Functor)

instance Applicative UpdateParser where
  pure x = UpdateParser (pure (pure x))
  UpdateParser f <*> UpdateParser x = UpdateParser (\u -> f u <*> x u)

instance Alternative UpdateParser where
  empty = UpdateParser (const Nothing)
  UpdateParser f <|> UpdateParser g = UpdateParser (\u -> f u <|> g u)

instance Monad UpdateParser where
  return = pure
  UpdateParser x >>= f = UpdateParser (\u -> x u >>= flip runUpdateParser u . f)
#if !MIN_VERSION_base(4,13,0)
  fail _ = empty
#endif

#if MIN_VERSION_base(4,13,0)
instance MonadFail UpdateParser where
  fail _ = empty
#endif

mkParser :: (Update -> Maybe a) -> UpdateParser a
mkParser = UpdateParser

parseUpdate :: UpdateParser a -> Update -> Maybe a
parseUpdate = runUpdateParser

text :: UpdateParser Text
text = UpdateParser (updateMessage >=> messageText)

plainText :: UpdateParser Text
plainText = do
  t <- text
  if "/" `Text.isPrefixOf` t
    then fail "command"
    else pure t

command :: Text -> UpdateParser Text
command name = do
  t <- text
  case Text.words t of
    (w:ws) | w == "/" <> name
      -> pure (Text.unwords ws)
    _ -> fail "not that command"

callbackQueryDataRead :: Read a => UpdateParser a
callbackQueryDataRead = mkParser $ \update -> do
  query <- updateCallbackQuery update
  data_ <- callbackQueryData query
  readMaybe (Text.unpack data_)

updateMessageText :: Update -> Maybe Text
updateMessageText = updateMessage >=> messageText
