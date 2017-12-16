{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Telegram.Bot.Handler where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Profunctor
import Data.String (IsString(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as Text

import Servant.Client

import Telegram.Bot.API

newtype Handlers m u a = Handlers { getHandlers :: u -> [m a] }
  deriving (Monoid)

instance Functor m => Profunctor (Handlers m) where
  dimap l r (Handlers hs) = Handlers (dimap l (map (fmap r)) hs)

instance Applicative m => Choice (Handlers m) where
  left' (Handlers hs) = Handlers (either (fmap (fmap Left) . hs) (pure . pure . Right))

handler :: (u -> m a) -> Handlers m u a
handler h = Handlers (pure . h)

handleAll :: Foldable f => (s -> f t) -> Handlers m t a -> Handlers m s a
handleAll f (Handlers hs) = Handlers (foldMap hs . f)

data Command = Command
  { commandName    :: Text
  , commandParams  :: [Text]
  , commandMessage :: Message
  }

updateCommand :: Text -> Update -> Maybe Command
updateCommand commandName Update{..} = do
  commandMessage <- updateMessage
  text <- messageText commandMessage
  case Text.words text of
    (w : commandParams)
      | w == "/" <> commandName -> Just Command{..}
    _ -> Nothing

command :: Text -> Handlers m Command a -> Handlers m Update a
command = handleAll . updateCommand

message :: Handlers m Message a -> Handlers m Update a
message = handleAll updateMessage

textMessage :: Handlers m Text a -> Handlers m Update a
textMessage = message . handleAll messageText

startPolling :: Handlers (ReaderT Update ClientM) Update a -> ClientM ()
startPolling Handlers{..} = go Nothing
  where
    go lastUpdateId = do
      let inc (UpdateId n) = UpdateId (n + 1)
          offset = fmap inc lastUpdateId
      res <- getUpdates (GetUpdatesRequest offset Nothing Nothing Nothing)
      let updates = responseResult res
          updateIds = map updateUpdateId updates
          maxUpdateId = maximum (Nothing : map Just updateIds)
      mapM_ (sequence_ . applyHandlers) updates
      liftIO $ threadDelay 1000000
      go maxUpdateId

    applyHandlers u = map (flip runReaderT u) (getHandlers u)

-- | Reply message parameters.
-- This is just like 'SendMessageRequest' but without 'SomeChatId' specified.
data ReplyMessage = ReplyMessage
  { replyMessageText                  :: Text -- ^ Text of the message to be sent.
  , replyMessageParseMode             :: Maybe ParseMode -- ^ Send 'Markdown' or 'HTML', if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , replyMessageDisableWebPagePreview :: Maybe Bool -- ^ Disables link previews for links in this message.
  , replyMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , replyMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , replyMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

instance IsString ReplyMessage where
  fromString s = ReplyMessage (fromString s) Nothing Nothing Nothing Nothing Nothing

replyMessageToSendMessageRequest :: SomeChatId -> ReplyMessage -> SendMessageRequest
replyMessageToSendMessageRequest someChatId ReplyMessage{..} = SendMessageRequest
  { sendMessageChatId = someChatId
  , sendMessageText = replyMessageText
  , sendMessageParseMode = replyMessageParseMode
  , sendMessageDisableWebPagePreview = replyMessageDisableWebPagePreview
  , sendMessageDisableNotification = replyMessageDisableNotification
  , sendMessageReplyToMessageId = replyMessageReplyToMessageId
  , sendMessageReplyMarkup = replyMessageReplyMarkup
  }

replyMessage :: SomeChatId -> ReplyMessage -> ClientM (Response Message)
replyMessage someChatId = sendMessage . replyMessageToSendMessageRequest someChatId

reply :: ReplyMessage -> ReaderT s (ReaderT Update ClientM) ()
reply rmsg = do
  u <- lift $ asks updateChatId
  case u of
    Just chatId -> do
      let msg = replyMessageToSendMessageRequest (SomeChatId chatId) rmsg
      lift $ lift $ sendMessage msg
      return ()
    Nothing -> return ()

hoistHandlers :: (m a -> n a) -> Handlers m u a -> Handlers n u a
hoistHandlers f (Handlers hs) = Handlers (map f . hs)
