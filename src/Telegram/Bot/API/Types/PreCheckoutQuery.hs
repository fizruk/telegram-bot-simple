{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.PreCheckoutQuery where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.OrderInfo
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'PreCheckoutQuery'

-- | This object contains information about an incoming pre-checkout query.
data PreCheckoutQuery = PreCheckoutQuery
  { preCheckoutQueryId               :: Text                   -- ^ Unique query identifier.
  , preCheckoutQueryFrom             :: User                   -- ^ User who sent the query.
  , preCheckoutQueryCurrency         :: Text                   -- ^ Three-letter ISO 4217 currency code
  , preCheckoutQueryTotalAmount      :: Int                  -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  , preCheckoutQueryInvoicePayload   :: Text                   -- ^ Bot specified invoice payload
  , preCheckoutQueryShippingOptionId :: Maybe ShippingOptionId -- ^ Identifier of the shipping option chosen by the user.
  , preCheckoutQueryOrderInfo        :: Maybe OrderInfo        -- ^ Order info provided by the user.
  }
  deriving (Generic, Show)

deriveJSON' ''PreCheckoutQuery
