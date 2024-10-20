{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.EditMessageLiveLocation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types
import Telegram.Bot.API.Internal.TH

-- ** 'editMessageLiveLocation'

-- | Request parameters for 'editMessageLiveLocation'.
data EditMessageLiveLocationRequest = EditMessageLiveLocationRequest
  { editMessageLiveLocationChatId :: Maybe SomeChatId -- ^ Unique identifier for the target chat or username of the target channel (in the format \@channelusername).
  , editMessageLiveLocationMessageId :: Maybe MessageId -- ^ Required if inline_message_id is not specified. Identifier of the message with live location to stop.
  , editMessageLiveLocationInlineMessageId :: Maybe InlineMessageId -- ^ Required if chat_id and message_id are not specified. Identifier of the inline message.
  , editMessageLiveLocationLatitude :: Float -- ^ Latitude of new location.
  , editMessageLiveLocationLongitude :: Float -- ^ Longitude of new location.
  , editMessageLiveLocationLivePeriod :: Maybe Int -- ^ New period in seconds during which the location can be updated, starting from the message send date. If @0x7FFFFFFF@ is specified, then the location can be updated forever. Otherwise, the new value must not exceed the current @live_period@ by more than a day, and the live location expiration date must remain within the next 90 days. If not specified, then @live_period@ remains unchanged.
  , editMessageLiveLocationHorizontalAccuracy :: Maybe Float -- ^ The radius of uncertainty for the location, measured in meters; 0-1500.
  , editMessageLiveLocationHeading :: Maybe Int -- ^ Direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  , editMessageLiveLocationProximityAlertRadius :: Maybe Int  -- ^ Maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  , editMessageLiveLocationReplyMarkup :: Maybe InlineKeyboardMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  }
  deriving Generic

instance ToJSON   EditMessageLiveLocationRequest where toJSON = gtoJSON
instance FromJSON EditMessageLiveLocationRequest where parseJSON = gparseJSON

type EditMessageLiveLocation = "editMessageLiveLocation"
  :> ReqBody '[JSON] EditMessageLiveLocationRequest
  :> Post '[JSON] (Response (Either Bool Message))

-- | Use this method to edit live location messages.
--   A location can be edited until its live_period
--   expires or editing is explicitly disabled by a
--   call to stopMessageLiveLocation. On success, if
--   the edited message is not an inline message, the
--   edited Message is returned, otherwise True is returned.
editMessageLiveLocation :: EditMessageLiveLocationRequest ->  ClientM (Response (Either Bool Message))
editMessageLiveLocation = client (Proxy @EditMessageLiveLocation)

makeDefault ''EditMessageLiveLocationRequest
