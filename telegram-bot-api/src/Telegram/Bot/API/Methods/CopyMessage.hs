{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.CopyMessage where

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
import Telegram.Bot.API.Internal.TH

-- ** 'copyMessage'

type CopyMessage
  = "copyMessage"
  :> ReqBody '[JSON] CopyMessageRequest
  :> Post '[JSON] (Response CopyMessageId)

-- | Use this method to copy messages of any kind.
--   Service messages and invoice messages can't be
--   copied. The method is analogous to the method
--   forwardMessage, but the copied message doesn't
--   have a link to the original message.
--   Returns the MessageId of the sent message on success.
copyMessage :: CopyMessageRequest ->  ClientM (Response CopyMessageId)
copyMessage = client (Proxy @CopyMessage)

-- | Request parameters for 'copyMessage'.
data CopyMessageRequest = CopyMessageRequest
  { copyMessageChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , copyMessageMessageThreadId :: Maybe Message -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , copyMessageFromChatId :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format @channelusername)
  , copyMessageMessageId :: MessageId -- ^ Message identifier in the chat specified in from_chat_id
  , copyMessageCaption :: Maybe Text -- ^ New caption for media, 0-1024 characters after entities parsing. If not specified, the original caption is kept
  , copyMessageParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , copyMessageCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , copyMessageDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , copyMessageProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , copyMessageReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , copyMessageReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , copyMessageReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   CopyMessageRequest where toJSON = gtoJSON
instance FromJSON CopyMessageRequest where parseJSON = gparseJSON

makeDefault ''CopyMessageRequest
