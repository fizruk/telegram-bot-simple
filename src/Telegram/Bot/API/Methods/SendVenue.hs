{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SendVenue where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'sendVenue'

-- | Request parameters for 'sendVenue'.
data SendVenueRequest = SendVenueRequest
  { sendVenueChatId :: SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendVenueMessageThreadId :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendVenueLatitude :: Float -- ^ Latitude of the venue
  , sendVenueLongitude :: Float -- ^ Longitude of the venue
  , sendVenueTitle :: Text -- ^ Name of the venue
  , sendVenueAddress :: Text -- ^ Address of the venue
  , sendVenueFoursquareId :: Maybe Text -- ^ Foursquare identifier of the venue
  , sendVenueFoursquareType :: Maybe Text -- ^ Foursquare type of the venue, if known. (For example, “arts_entertainment/default”, “arts_entertainment/aquarium” or “food/icecream”.)
  , sendVenueGooglePlaceId :: Maybe Text -- ^ Google Places identifier of the venue
  , sendVenueGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See supported types <https:\/\/developers.google.com\/maps\/documentation\/places\/web-service\/supported_types>.)
  , sendVenueDisableNotification :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendVenueProtectContent :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving
  , sendVenueReplyToMessageId :: Maybe MessageId -- ^ If the message is a reply, ID of the original message
  , sendVenueAllowSendingWithoutReply :: Maybe Bool -- ^ Pass True, if the message should be sent even if the specified replied-to message is not found
  , sendVenueReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   SendVenueRequest where toJSON = gtoJSON
instance FromJSON SendVenueRequest where parseJSON = gparseJSON

type SendVenue = "sendVenue"
  :> ReqBody '[JSON] SendVenueRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send information about a venue.
--   On success, the sent Message is returned.
sendVenue :: SendVenueRequest ->  ClientM (Response Message)
sendVenue = client (Proxy @SendVenue)
