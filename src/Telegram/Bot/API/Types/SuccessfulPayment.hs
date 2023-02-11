{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.SuccessfulPayment where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.OrderInfo
import Telegram.Bot.API.Internal.Utils

-- ** 'SuccessfulPayment'

-- | This object contains basic information about a successful payment.
data SuccessfulPayment = SuccessfulPayment
  { successfulPaymentCurrency                :: Text                   -- ^ Three-letter ISO 4217 currency code.
  , successfulPaymentTotalAmount             :: Int                  -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  , successfulPaymentInvoicePayload          :: Text                   -- ^ Bot specified invoice payload.
  , successfulPaymentShippingOptionId        :: Maybe ShippingOptionId -- ^ Identifier of the shipping option chosen by the user.
  , successfulPaymentOrderInfo               :: Maybe OrderInfo        -- ^ Order info provided by the user.
  , successfulPaymentTelegramPaymentChargeId :: Text                   -- ^ Telegram payment identifier.
  , successfulPaymentProviderPaymentChargeId :: Text                   -- ^ Provider payment identifier.
  }
  deriving (Generic, Show)

instance ToJSON   SuccessfulPayment where toJSON = gtoJSON
instance FromJSON SuccessfulPayment where parseJSON = gparseJSON
