{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.ForwardMessages where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'forwardMessages'

type ForwardMessages
  = "forwardMessages" :> ReqBody '[JSON] ForwardMessagesRequest :> Post '[JSON] (Response [MessageId])

-- | Use this method to forward multiple messages of any kind.
-- If some of the specified messages can't be found or forwarded, they are skipped.
-- Service messages and messages with protected content can't be forwarded.
-- Album grouping is kept for forwarded messages.
-- On success, an array of 'MessageId' of the sent messages is returned.
forwardMessages :: ForwardMessagesRequest -> ClientM (Response [MessageId])
forwardMessages = client (Proxy @ForwardMessages)


-- | Request parameters for 'forwardMessages'.
data ForwardMessagesRequest = ForwardMessagesRequest
  { forwardMessagesChatId              :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername).
  , forwardMessagesMessageThreadId     :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , forwardMessagesFromChatId          :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @\@channelusername).
  , forwardMessagesMessageIds          :: [MessageId]  -- ^ Identifiers of 1-100 messages in the chat from_chat_id to forward. The identifiers must be specified in a strictly increasing order.
  , forwardMessagesDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , forwardMessagesProtectContent      :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  } deriving (Generic)

instance ToJSON   ForwardMessagesRequest where toJSON = gtoJSON
instance FromJSON ForwardMessagesRequest where parseJSON = gparseJSON

makeDefault ''ForwardMessagesRequest
