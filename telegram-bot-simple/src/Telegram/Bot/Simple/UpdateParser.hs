{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Telegram.Bot.Simple.UpdateParser where

import           Control.Monad
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
import           Control.Monad.Reader
import           Telegram.Bot.API

type UpdateParser a = ReaderT Update Maybe a

parseUpdate :: UpdateParser a -> Update -> Maybe a
parseUpdate = runReaderT

text :: UpdateParser Text
text = ask >>= (lift . (extractUpdateMessage >=> messageText))

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
callbackQueryDataRead = ask >>= (lift . (updateCallbackQuery >=> callbackQueryData >=> (readMaybe . Text.unpack)))

updateMessageText :: Update -> Maybe Text
updateMessageText = extractUpdateMessage >=> messageText


updateMessageSticker :: Update -> Maybe Sticker
updateMessageSticker = extractUpdateMessage >=> messageSticker
