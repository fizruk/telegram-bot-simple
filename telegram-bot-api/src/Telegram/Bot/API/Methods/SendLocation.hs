{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendLocation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'sendLocation'

-- | Request parameters for 'sendLocation'.
data SendLocationRequest = SendLocationRequest
  { sendLocationBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , sendLocationChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendLocationMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendLocationLatitude :: Float -- ^ Latitude of new location
  , sendLocationLongitude :: Float -- ^ Longitude of new location
  , sendLocationHorizontalAccuracy :: Maybe Float -- ^ The radius of uncertainty for the location, measured in meters; 0-1500
  , sendLocationLivePeriod :: Int -- ^ Period in seconds for which the location will be updated (see Live Locations, should be between 60 and 86400.)
  , sendLocationHeading :: Maybe Int -- ^ Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  , sendLocationProximityAlertRadius :: Maybe Int  -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  , sendLocationDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendLocationProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendLocationReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendLocationReplyParameters :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , sendLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   SendLocationRequest where toJSON = gtoJSON
instance FromJSON SendLocationRequest where parseJSON = gparseJSON


type SendLocation = "sendLocation"
  :> ReqBody '[JSON] SendLocationRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send point on the map.
--   On success, the sent Message is returned.
sendLocation :: SendLocationRequest ->  ClientM (Response Message)
sendLocation = client (Proxy @SendLocation)

makeDefault ''SendLocationRequest
