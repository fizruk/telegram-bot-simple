{-# LANGUAGE DeriveGeneric              #-}
module Telegram.Bot.API.InlineMode.InputMessageContent (InputMessageContent(..), defaultInputTextMessageContent, defaultInputLocationMessageContent) where

import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Telegram.Bot.API.Internal.Utils

-- | Represents the content of a text message to be sent as the result of an inline query.
data InputMessageContent =
  InputTextMessageContent -- ^ Represents the [content](https://core.telegram.org/bots/api#inputmessagecontent) of a text message to be sent as the result of an inline query.
  { inputMessageContentMessageText :: Text -- ^ Text of the message to be sent, 1-4096 characters
  , inputMessageContentParseMode :: Maybe Text -- ^ Mode for parsing entities in the message text. See [formatting options](https://core.telegram.org/bots/api#formatting-options) for more details.
  , inputMessageContentDisableWebPagePrefiew :: Maybe Bool -- ^ Disables link previews for links in the sent message
  }
  | InputLocationMessageContent                                      -- ^ Represents the [content](https://core.telegram.org/bots/api#inputmessagecontent) of a location message to be sent as the result of an inline query.
  { inputMessageContentLatitude :: Float                     -- ^ Latitude of the location in degrees
  , inputMessageContentLongitude :: Float                    -- ^ Longitude of the location in degrees
  , inputMessageContentHorizontalAccuracy :: Maybe Float     -- ^ The radius of uncertainty for the location, measured in meters; 0-1500
  , inputMessageContentLivePeriod :: Maybe Integer           -- ^ Period in seconds for which the location can be updated, should be between 60 and 86400.
  , inputMessageContentHeading :: Maybe Integer              -- ^ For live locations, a direction in which the user is moving, in degrees. Must be between 1 and 360 if specified.
  , inputMessageContentProximityAlertRadius :: Maybe Integer -- ^ For live locations, a maximum distance for proximity alerts about approaching another chat member, in meters. Must be between 1 and 100000 if specified.
  }
  | InputVenueMessageContent                              -- ^ Represents the content of a [venue](https://core.telegram.org/bots/api#inputmessagecontent) message to be sent as the result of an inline query.
  { inputMessageContentLatitude :: Float             -- ^ Latitude of the venue in degrees
  , inputMessageContentLongitude :: Float            -- ^ Longitude of the venue in degrees
  , inputMessageContentTitle :: Text                 -- ^ Name of the venue
  , inputMessageContentAddress :: Text               -- ^ Address of the venue
  , inputMessageContentFoursquareId :: Maybe Text    -- ^ Foursquare identifier of the venue, if known
  , inputMessageContentFoursquareType :: Maybe Text  -- ^ Foursquare type of the venue, if known. (For example, “arts_entertainment\/default”, “arts_entertainment\/aquarium” or “food\/icecream”.)
  , inputMessageContentGooglePlaceId :: Maybe Text   -- ^ Google Places identifier of the venue
  , inputMessageContentGooglePlaceType :: Maybe Text -- ^ Google Places type of the venue. (See [supported types](https://developers.google.com/places/web-service/supported_types).)
  }
  | InputContactMessageContent                         -- ^ Represents the [content](https://core.telegram.org/bots/api#inputmessagecontent) of a contact message to be sent as the result of an inline query.
  { inputMessageContentPhoneNumber :: Text      -- ^ Contact's phone number
  , inputMessageContentFirstName :: Text        -- ^ Contact's first name
  , inputMessageContentSecondName :: Maybe Text -- ^ Contact's last name
  , inputMessageContentVcard :: Maybe Text      -- ^ Additional data about the contact in the form of a [vCard](https://en.wikipedia.org/wiki/VCard), 0-2048 bytes
  } deriving (Generic, Show)

-- ** Helper functions to easily construct 'InputMessageContent'

defaultInputTextMessageContent :: Text -> InputMessageContent
defaultInputTextMessageContent text = InputTextMessageContent text Nothing Nothing

defaultInputLocationMessageContent :: Float -> Float -> InputMessageContent
defaultInputLocationMessageContent lat long = InputLocationMessageContent lat long Nothing Nothing Nothing Nothing 

instance ToJSON InputMessageContent where toJSON = gtoJSON
instance FromJSON InputMessageContent where parseJSON = gparseJSON
