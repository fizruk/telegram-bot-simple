{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.ForwardMessage where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'forwardMessage'

type ForwardMessage
  = "forwardMessage" :> ReqBody '[JSON] ForwardMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to forward messages of any kind.
-- On success, the sent 'Message' is returned.

forwardMessage :: ForwardMessageRequest -> ClientM (Response Message)
forwardMessage = client (Proxy @ForwardMessage)


-- | Request parameters for 'forwardMessage'.
data ForwardMessageRequest = ForwardMessageRequest
  { forwardMessageChatId              :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername).
  , forwardMessageMessageThreadId     :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , forwardMessageFromChatId          :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @\@channelusername).
  , forwardMessageDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , forwardMessageProtectContent      :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , forwardMessageMessageId           :: MessageId  -- ^ Message identifier in the chat specified in from_chat_id.
  } deriving (Generic)

instance ToJSON   ForwardMessageRequest where toJSON = gtoJSON
instance FromJSON ForwardMessageRequest where parseJSON = gparseJSON
