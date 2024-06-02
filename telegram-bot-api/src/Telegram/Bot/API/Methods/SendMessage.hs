{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendMessage where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Types.ParseMode
import Telegram.Bot.API.Types.SomeReplyMarkup
import Telegram.Bot.API.Internal.TH

-- ** 'sendMessage'

type SendMessage
  = "sendMessage" :> ReqBody '[JSON] SendMessageRequest :> Post '[JSON] (Response Message)

-- | Use this method to send text messages.
-- On success, the sent 'Message' is returned.
sendMessage :: SendMessageRequest -> ClientM (Response Message)
sendMessage = client (Proxy @SendMessage)

-- | Request parameters for 'sendMessage'.
data SendMessageRequest = SendMessageRequest
  { sendMessageBusinessConnectionId  :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendMessageChatId                :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @\@channelusername@).
  , sendMessageMessageThreadId       :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendMessageText                  :: Text -- ^ Text of the message to be sent.
  , sendMessageParseMode             :: Maybe ParseMode -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , sendMessageEntities              :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in message text, which can be specified instead of /parse_mode/.
  , sendMessageLinkPreviewOptions    :: Maybe LinkPreviewOptions -- ^ Link preview generation options for the message.
  , sendMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendMessageProtectContent        :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendMessageMessageEffectId       :: Maybe Text -- ^ Unique identifier of the message effect to be added to the message; for private chats only.
  , sendMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , sendMessageReplyParameters       :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

instance ToJSON   SendMessageRequest where toJSON = gtoJSON
instance FromJSON SendMessageRequest where parseJSON = gparseJSON

makeDefault ''SendMessageRequest
