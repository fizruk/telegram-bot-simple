{-# LANGUAGE DeriveGeneric              #-}
module Telegram.Bot.API.InlineMode.InputMessageContent (InputMessageContent(..), defaultInputTextMessageContent, defaultInputLocationMessageContent) where

import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)

import           Telegram.Bot.API.Internal.Utils
import           Telegram.Bot.API.Types.LabeledPrice

-- | Represents the content of a text message to be sent as the result of an inline query.
data InputMessageContent
  = InputTextMessageContent -- ^ Represents the [content](https://core.telegram.org/bots/api#inputmessagecontent) of a text message to be sent as the result of an inline query.
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
    }
  | InputInvoiceMessageContent                        -- ^ Represents the [content](https://core.telegram.org/bots/api#inputmessagecontent) of an invoice message to be sent as the result of an inline query.
    { inputMessageContentTitle :: Text -- ^ Product name, 1-32 characters.
    , inputMessageContentDescription :: Text -- ^ Product description, 1-255 characters.
    , inputMessageContentPayload :: Text -- ^ Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.
    , inputMessageContentProviderToken :: Text -- ^ Payment provider token, obtained via [@BotFather](https://t.me/botfather).
    , inputMessageContentCurrency :: Text -- ^ Three-letter ISO 4217 currency code, see [more on currencies](https://core.telegram.org/bots/payments#supported-currencies).
    , inputMessageContentPrices :: [LabeledPrice] -- ^ Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.).
    , inputMessageContentMaxTipAmount :: Maybe Integer -- ^ The maximum accepted amount for tips in the smallest units of the currency (integer, not float/double). For example, for a maximum tip of @US$ 1.45@ pass @max_tip_amount = 145@. See the exp parameter in [currencies.json](https://core.telegram.org/bots/payments/currencies.json), it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0.
    , inputMessageContentSuggestedTipAmounts :: Maybe [Integer] -- ^ A JSON-serialized array of suggested amounts of tip in the smallest units of the currency (integer, not float/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed @max_tip_amount@.
    , inputMessageContentProviderData :: Maybe Text -- ^ A JSON-serialized object for data about the invoice, which will be shared with the payment provider. A detailed description of the required fields should be provided by the payment provider.
    , inputMessageContentPhotoUrl :: Maybe Text -- ^ URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service.
    , inputMessageContentPhotoSize :: Maybe Integer -- ^ Photo size in bytes.
    , inputMessageContentPhotoWidth :: Maybe Integer -- ^ Photo width.
    , inputMessageContentPhotoHeight :: Maybe Integer -- ^ Photo height.
    , inputMessageContentNeedName :: Maybe Bool -- ^ 'True' if you require the user's full name to complete the order.
    , inputMessageContentNeedPhoneNumber :: Maybe Bool -- ^ 'True' if you require the user's phone number to complete the order.
    , inputMessageContentNeedEmail :: Maybe Bool -- ^ 'True' if you require the user's email address to complete the order.
    , inputMessageContentNeedShippingAddress :: Maybe Bool -- ^ 'True' if you require the user's shipping address to complete the order.
    , inputMessageContentSendPhoneNumberToProvider :: Maybe Bool -- ^ 'True' if the user's phone number should be sent to provider.
    , inputMessageContentSendEmailToProvider :: Maybe Bool -- ^ 'True' if the user's email address should be sent to provider.
    , inputMessageContentIsFlexible :: Maybe Bool -- ^ 'True' if the final price depends on the shipping method.
    }
  deriving (Generic, Show)

-- ** Helper functions to easily construct 'InputMessageContent'

defaultInputTextMessageContent :: Text -> InputMessageContent
defaultInputTextMessageContent text = InputTextMessageContent text Nothing Nothing

defaultInputLocationMessageContent :: Float -> Float -> InputMessageContent
defaultInputLocationMessageContent lat long = InputLocationMessageContent lat long Nothing Nothing Nothing Nothing 

instance ToJSON InputMessageContent where toJSON = gtoJSON
instance FromJSON InputMessageContent where parseJSON = gparseJSON
