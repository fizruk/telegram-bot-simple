{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.PinChatMessage where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'pinChatMessage'

-- | Request parameters for 'pinChatMessage'.
data PinChatMessageRequest = PinChatMessageRequest
  { pinChatMessageChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername)
  , pinChatMessageMessageId :: MessageId -- ^ Identifier of a message to pin
  , pinChatMessageDisableNotification :: Maybe Bool -- ^ Pass True, if it is not necessary to send a notification to all chat members about the new pinned message. Notifications are always disabled in channels and private chats.
  }
  deriving Generic

instance ToJSON   PinChatMessageRequest where toJSON = gtoJSON
instance FromJSON PinChatMessageRequest where parseJSON = gparseJSON

type PinChatMessage = "pinChatMessage"
  :> ReqBody '[JSON] PinChatMessageRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to add a message to the list 
--   of pinned messages in a chat. If the chat is 
--   not a private chat, the bot must be an administrator 
--   in the chat for this to work and must have the 
--   'can_pin_messages' administrator right in a supergroup
--   or 'can_edit_messages' administrator right in a channel. 
--   Returns True on success.
pinChatMessage :: PinChatMessageRequest ->  ClientM (Response Bool)
pinChatMessage = client (Proxy @PinChatMessage)
