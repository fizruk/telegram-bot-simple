{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.CopyMessages where

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

-- ** 'copyMessages'

type CopyMessages
  = "copyMessages"
  :> ReqBody '[JSON] CopyMessagesRequest
  :> Post '[JSON] (Response [CopyMessageId])

-- | Use this method to copy messages of any kind.
-- If some of the specified messages can't be found or copied, they are skipped.
-- Service messages, giveaway messages, giveaway winners messages,
-- and invoice messages can't be copied.
-- A quiz poll can be copied only if the value of the field @correct_option_id@ is knownto the bot.
-- The method is analogous to the method 'forwardMessages',
-- but the copied messages don't have a link to the original message.
-- Album grouping is kept for copied messages.
-- On success, an array of 'CopyMessageId' of the sent messages is returned.
copyMessages :: CopyMessagesRequest ->  ClientM (Response [CopyMessageId])
copyMessages = client (Proxy @CopyMessages)

-- | Request parameters for 'copyMessages'.
data CopyMessagesRequest = CopyMessagesRequest
  { copyMessagesChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , copyMessagesMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , copyMessagesFromChatId :: SomeChatId -- ^ Unique identifier for the chat where the original message was sent (or channel username in the format \@channelusername).
  , copyMessagesMessageIds :: [MessageId] -- ^ Identifiers of 1-100 messages in the chat @from_chat_id@ to copy. The identifiers must be specified in a strictly increasing order.
  , copyMessagesCaption :: Maybe Text -- ^ New caption for media, 0-1024 characters after entities parsing. If not specified, the original caption is kept
  , copyMessagesParseMode :: Maybe ParseMode  -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , copyMessagesCaptionEntities :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in the caption, which can be specified instead of parse_mode
  , copyMessagesDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , copyMessagesProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , copyMessagesRemoveCaption :: Maybe Bool -- ^ Pass 'True' to copy the messages without their captions.
  }
  deriving Generic

instance ToJSON   CopyMessagesRequest where toJSON = gtoJSON
instance FromJSON CopyMessagesRequest where parseJSON = gparseJSON

makeDefault ''CopyMessagesRequest
