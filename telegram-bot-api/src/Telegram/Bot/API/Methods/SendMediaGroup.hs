{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendMediaGroup where

import Data.Aeson (ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'sendMediaGroup'

-- | Request parameters for 'sendMediaGroup'.
data SendMediaGroupRequest = SendMediaGroupRequest
  { sendMediaGroupChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendMediaGroupMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendMediaGroupMedia :: [InputMedia] -- ^ A JSON-serialized array describing messages to be sent, must include 2-10 items. InputMediaAudio, InputMediaDocument, InputMediaPhoto or InputMediaVideo.
  , sendMediaGroupDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendMediaGroupProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendMediaGroupReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendMediaGroupAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendMediaGroupReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON SendMediaGroupRequest where toJSON = gtoJSON

type SendMediaGroup = "sendMediaGroup"
  :> ReqBody '[JSON] SendMediaGroupRequest
  :> Post '[JSON] (Response [Message])

-- | Use this method to send a group of photos, videos,
--   documents or audios as an album. Documents
--   and audio files can be only grouped in an album
--   with messages of the same type.
--   On success, an array of Messages that were sent is returned.
sendMediaGroup :: SendMediaGroupRequest ->  ClientM (Response [Message])
sendMediaGroup = client (Proxy @SendMediaGroup)

makeDefault ''SendMediaGroupRequest
