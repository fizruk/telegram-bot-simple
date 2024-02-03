{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendDice where

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

-- ** 'sendDice'

-- | Request parameters for 'sendDice'.
data SendDiceRequest = SendDiceRequest
  { sendDiceChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendDiceMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendDiceEmoji :: Maybe Text -- ^ Emoji on which the dice throw animation is based. Currently, must be one of “🎲”, “🎯”, “🏀”, “⚽”, “🎳”, or “🎰”. Dice can have values 1-6 for “🎲”, “🎯” and “🎳”, values 1-5 for “🏀” and “⚽”, and values 1-64 for “🎰”. Defaults to “🎲”
  , sendDiceDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendDiceProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding
  , sendDiceReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendDiceReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendDiceReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   SendDiceRequest where toJSON = gtoJSON
instance FromJSON SendDiceRequest where parseJSON = gparseJSON

type SendDice = "sendDice"
  :> ReqBody '[JSON] SendDiceRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send an animated emoji that
--   will display a random value.
--   On success, the sent Message is returned.
sendDice :: SendDiceRequest ->  ClientM (Response Message)
sendDice = client (Proxy @SendDice)

makeDefault ''SendDiceRequest
