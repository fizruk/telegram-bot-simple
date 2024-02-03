{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendContact where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'sendContact'

-- | Request parameters for 'sendContact'.
data SendContactRequest = SendContactRequest
  { sendContactChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendContactMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendContactPhoneNumber :: Text -- ^ Contact's phone number
  , sendContactFirstName  :: Text -- ^ Contact's first name
  , sendContactLastName  :: Text -- ^ Contact's last name
  , sendContactVcard  :: Text -- ^ Additional data about the contact in the form of a vCard, 0-2048 bytes
  , sendContactDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendContactProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendContactReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendContactReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendContactReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   SendContactRequest where toJSON = gtoJSON
instance FromJSON SendContactRequest where parseJSON = gparseJSON

type SendContact = "sendContact"
  :> ReqBody '[JSON] SendContactRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send phone contacts.
--   On success, the sent Message is returned.
sendContact :: SendContactRequest ->  ClientM (Response Message)
sendContact = client (Proxy @SendContact)

makeDefault ''SendContactRequest
