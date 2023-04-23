{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Telegram.Bot.Simple.UpdateParser where

import           Control.Applicative
import           Control.Monad.Reader
#if defined(MIN_VERSION_GLASGOW_HASKELL)
#if MIN_VERSION_GLASGOW_HASKELL(8,6,2,0)
#else
import           Data.Monoid                     ((<>))
#endif
#endif
import qualified Data.Char            as Char
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
text = UpdateParser (extractUpdateMessage >=> messageText)

plainText :: UpdateParser Text
plainText = do
  t <- text
  if "/" `Text.isPrefixOf` t
    then fail "command"
    else pure t

command :: Text -> UpdateParser Text
command name = do
  t <- text
  let (cmd, rest) = Text.break Char.isSpace t
  if cmd == "/" <> name
    then pure $ Text.stripStart rest
    else fail "not that command"

commandWithBotName :: Text -> Text -> UpdateParser Text
commandWithBotName botname commandname = do
  t <- text
  let (cmd, rest) = Text.break Char.isSpace t
  if cmd `elem` ["/" <> commandname <> "@" <> botname, "/" <> commandname]
    then pure $ Text.stripStart rest
    else fail "not that command"

-- | Obtain 'CallbackQuery' @data@ associated with the callback button in an inline keyboard if present in 'Update' message. 
callbackQueryDataRead :: Read a => UpdateParser a
callbackQueryDataRead = mkParser $ \update -> do
  query <- updateCallbackQuery update
  data_ <- callbackQueryData query
  readMaybe (Text.unpack data_)

updateMessageText :: Update -> Maybe Text
updateMessageText = extractUpdateMessage >=> messageText


updateMessageSticker :: Update -> Maybe Sticker
updateMessageSticker = extractUpdateMessage >=> messageSticker
