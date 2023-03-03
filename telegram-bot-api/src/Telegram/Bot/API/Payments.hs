{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Telegram.Bot.API.Payments where

import Data.Aeson
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH (makeDefault)

-- * Methods

-- ** 'sendInvoice'

data SendInvoiceRequest = SendInvoiceRequest
  { sendInvoiceChatId                    :: ChatId                     -- ^ Unique identifier for the target chat or username of the target channel (in the format @channelusername).
  , sendInvoiceMessageThreadId           :: Maybe MessageThreadId      -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , sendInvoiceTitle                     :: Text                       -- ^ Product name, 1-32 characters.
  , sendInvoiceDescription               :: Text                       -- ^ Product description, 1-255 characters.
  , sendInvoicePayload                   :: Text                       -- ^ Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.
  , sendInvoiceProviderToken             :: Text                       -- ^ Payments provider token, obtained via Botfather.
  , sendInvoiceCurrency                  :: Text                       -- ^ Three-letter ISO 4217 currency code, see more on currencies.
  , sendInvoicePrices                    :: [LabeledPrice]             -- ^ Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.).
  , sendInvoiceMaxTipAmount              :: Maybe Integer              -- ^ The maximum accepted amount for tips in the smallest units of the currency (integer, not float\/double). For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0.
  , sendInvoiceequestSuggestedTipAmounts :: Maybe [Integer]            -- ^ A JSON-serialized array of suggested amounts of tips in the smallest units of the currency (integer, not float/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed max_tip_amount.
  , sendInvoiceStartParameter            :: Maybe Text                 -- ^ Unique deep-linking parameter. If left empty, forwarded copies of the sent message will have a Pay button, allowing multiple users to pay directly from the forwarded message, using the same invoice. If non-empty, forwarded copies of the sent message will have a URL button with a deep link to the bot (instead of a Pay button), with the value used as the start parameter.
  , sendInvoiceProviderData              :: Maybe Text                 -- ^ A JSON-serialized data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.
  , sendInvoicePhotoUrl                  :: Maybe Text                 -- ^ URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service. People like it better when they see what they are paying for.
  , sendInvoicePhotoSize                 :: Maybe Int                  -- ^ Photo size.
  , sendInvoicePhotoWidth                :: Maybe Int                  -- ^ Photo width.
  , sendInvoicePhotoHeight               :: Maybe Int                  -- ^ Photo height.
  , sendInvoiceNeedName                  :: Maybe Bool                 -- ^ Pass 'True', if you require the user's full name to complete the order.
  , sendInvoiceNeedPhoneNumber           :: Maybe Bool                 -- ^ Pass 'True', if you require the user's phone number to complete the order.
  , sendInvoiceNeedEmail                 :: Maybe Bool                 -- ^ Pass 'True', if you require the user's email address to complete the order.
  , sendInvoiceNeedShippingAddress       :: Maybe Bool                 -- ^ Pass 'True', if you require the user's shipping address to complete the order.
  , sendInvoiceSendPhoneNumberToProvider :: Maybe Bool                 -- ^ Pass 'True', if you require the user's phone number to complete the order.
  , sendInvoiceSendEmailToProvider       :: Maybe Bool                 -- ^ Pass 'True', if user's email address should be sent to provider.
  , sendInvoiceIsFlexible                :: Maybe Bool                 -- ^ Pass 'True', if the final price depends on the shipping method.
  , sendInvoiceDisableNotification       :: Maybe Bool                 -- ^ Sends the message silently. Users will receive a notification with no sound.
  , sendInvoiceProtectContent            :: Maybe Bool                 -- ^ Protects the contents of the sent message from forwarding and saving.
  , sendInvoiceReplyToMessageId          :: Maybe MessageId            -- ^ If the message is a reply, ID of the original message.
  , sendInvoiceAllowSendingWithoutReply  :: Maybe Bool                 -- ^ Pass 'True', if the message should be sent even if the specified replied-to message is not found.
  , sendInvoiceReplyMarkup               :: Maybe InlineKeyboardMarkup -- ^ A JSON-serialized object for an inline keyboard. If empty, one 'Pay total price' button will be shown. If not empty, the first button must be a Pay button.
  }
  deriving (Generic, Show)

instance ToJSON SendInvoiceRequest where toJSON = gtoJSON
instance FromJSON SendInvoiceRequest where parseJSON = gparseJSON

type SendInvoice
  =  "sendInvoice"
  :> ReqBody '[JSON] SendInvoiceRequest
  :> Post '[JSON] (Response Message)

-- | Use this method to send invoices. On success, the sent 'Message' is returned.
sendInvoice :: SendInvoiceRequest -> ClientM (Response Message)
sendInvoice = client (Proxy @SendInvoice)

-- ** 'createInvoiceLink'

data CreateInvoiceLinkRequest = CreateInvoiceLinkRequest
  { createInvoiceLinkTitle                     :: Text            -- ^ Product name, 1-32 characters.
  , createInvoiceLinkDescription               :: Text            -- ^ Product description, 1-255 characters.
  , createInvoiceLinkPayload                   :: Text            -- ^ Bot-defined invoice payload, 1-128 bytes. This will not be displayed to the user, use for your internal processes.
  , createInvoiceLinkProviderToken             :: Text            -- ^ Payment provider token, obtained via BotFather.
  , createInvoiceLinkCurrency                  :: Text            -- ^ Three-letter ISO 4217 currency code, see more on currencies.
  , createInvoiceLinkPrices                    :: [LabeledPrice]  -- ^ Price breakdown, a JSON-serialized list of components (e.g. product price, tax, discount, delivery cost, delivery tax, bonus, etc.).
  , createInvoiceLinkMaxTipAmount              :: Maybe Integer   -- ^ The maximum accepted amount for tips in the smallest units of the currency (integer, not float\/double). For example, for a maximum tip of US$ 1.45 pass max_tip_amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies). Defaults to 0.
  , createInvoiceLinkSuggestedTipAmounts       :: Maybe [Integer] -- ^ A JSON-serialized array of suggested amounts of tips in the smallest units of the currency (integer, not float\/double). At most 4 suggested tip amounts can be specified. The suggested tip amounts must be positive, passed in a strictly increased order and must not exceed @max_tip_amount@.
  , createInvoiceLinkProviderData              :: Maybe Text      -- ^ JSON-serialized data about the invoice, which will be shared with the payment provider. A detailed description of required fields should be provided by the payment provider.
  , createInvoiceLinkPhotoUrl                  :: Maybe Text      -- ^ URL of the product photo for the invoice. Can be a photo of the goods or a marketing image for a service.
  , createInvoiceLinkPhotoSize                 :: Maybe Int       -- ^ Photo size in bytes.
  , createInvoiceLinkPhotoWidth                :: Maybe Int       -- ^ Photo width.
  , createInvoiceLinkPhotoHeight               :: Maybe Int       -- ^ Photo height
  , createInvoiceLinkNeedName                  :: Maybe Bool      -- ^ Pass 'True' if you require the user's full name to complete the order.
  , createInvoiceLinkNeedPhoneNumber           :: Maybe Bool      -- ^ Pass 'True' if you require the user's phone number to complete the order.
  , createInvoiceLinkNeedEmail                 :: Maybe Bool      -- ^ Pass 'True' if you require the user's email address to complete the order.
  , createInvoiceLinkNeedShippingAddress       :: Maybe Bool      -- ^ Pass 'True' if you require the user's shipping address to complete the order.
  , createInvoiceLinkSendPhoneNumberToProvider :: Maybe Bool      -- ^ Pass 'True' if the user's phone number should be sent to the provider.
  , createInvoiceLinkSendEmailToProvider       :: Maybe Bool      -- ^ Pass 'True' if the user's email address should be sent to the provider.
  , createInvoiceLinkIsFlexible                :: Maybe Bool      -- ^ Pass 'True' if the final price depends on the shipping method.
  }
  deriving (Generic, Show)

instance ToJSON CreateInvoiceLinkRequest where toJSON = gtoJSON
instance FromJSON CreateInvoiceLinkRequest where parseJSON = gparseJSON

type CreateInvoiceLink
  =  "createInvoiceLink"
  :> ReqBody '[JSON] CreateInvoiceLinkRequest
  :> Post '[JSON] (Response Text)

-- | Use this method to create a link for an invoice. Returns the created invoice link as 'Text' on success.
createInvoiceLink :: CreateInvoiceLinkRequest -> ClientM (Response Text)
createInvoiceLink = client (Proxy @CreateInvoiceLink)

-- ** 'answerShippingQuery'

data AnswerShippingQueryRequest = AnswerShippingQueryRequest
  { answerShippingQueryShippingQueryId :: Text                   -- ^ Unique identifier for the query to be answered.
  , answerShippingQueryOk              :: Bool                   -- ^ Specify 'True' if delivery to the specified address is possible and 'False' if there are any problems (for example, if delivery to the specified address is not possible).
  , answerShippingQueryShippingOptions :: Maybe [ShippingOption] -- ^ Required if @ok@ is 'True'. A JSON-serialized array of available shipping options.
  , answerShippingQueryErrorMessage    :: Maybe Text             -- ^ Required if @ok@ is 'False'. Error message in human readable form that explains why it is impossible to complete the order (e.g. "Sorry, delivery to your desired address is unavailable'). Telegram will display this message to the user.
  }
  deriving (Generic, Show)

instance ToJSON AnswerShippingQueryRequest where toJSON = gtoJSON
instance FromJSON AnswerShippingQueryRequest where parseJSON = gparseJSON

type AnswerShippingQuery
  =  "answerShippingQuery"
  :> ReqBody '[JSON] AnswerShippingQueryRequest
  :> Post '[JSON] (Response Bool)

-- | If you sent an invoice requesting a shipping address and the parameter @is_flexible@ was specified, the Bot API will send an 'Update' with a @shipping_query@ field to the bot. Use this method to reply to shipping queries. On success, True is returned.
answerShippingQuery :: AnswerShippingQueryRequest -> ClientM (Response Bool)
answerShippingQuery = client (Proxy @AnswerShippingQuery)

-- ** 'answerPreCheckoutQuery'

data AnswerPreCheckoutQueryRequest = AnswerPreCheckoutQueryRequest
  { answerPreCheckoutQueryPreCheckoutQueryId :: Text       -- ^ Unique identifier for the query to be answered.
  , answerPreCheckoutQueryOk                 :: Bool       -- ^ Specify 'True' if everything is alright (goods are available, etc.) and the bot is ready to proceed with the order. Use False if there are any problems.
  , answerPreCheckoutQueryErrorMessage       :: Maybe Text -- ^ Required if @ok@ is 'False'. Error message in human readable form that explains the reason for failure to proceed with the checkout (e.g. "Sorry, somebody just bought the last of our amazing black T-shirts while you were busy filling out your payment details. Please choose a different color or garment!"). Telegram will display this message to the user.
  }
  deriving (Generic, Show)

instance ToJSON AnswerPreCheckoutQueryRequest where toJSON = gtoJSON
instance FromJSON AnswerPreCheckoutQueryRequest where parseJSON = gparseJSON

type AnswerPreCheckoutQuery
  =  "answerPreCheckoutQuery"
  :> ReqBody '[JSON] AnswerPreCheckoutQueryRequest
  :> Post '[JSON] (Response Bool)

-- | Once the user has confirmed their payment and shipping details, the Bot API sends the final confirmation in the form of an Update with the field pre_checkout_query. Use this method to respond to such pre-checkout queries. On success, 'True' is returned. Note: The Bot API must receive an answer within 10 seconds after the pre-checkout query was sent.
answerPreCheckoutQuery :: AnswerPreCheckoutQueryRequest -> ClientM (Response Bool)
answerPreCheckoutQuery = client (Proxy @AnswerPreCheckoutQuery)

foldMap makeDefault
  [ ''SendInvoiceRequest
  , ''CreateInvoiceLinkRequest
  , ''AnswerShippingQueryRequest
  , ''AnswerPreCheckoutQueryRequest
  ]
