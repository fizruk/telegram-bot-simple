{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Invoice where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'Invoice'

-- | This object contains basic information about an invoice.
data Invoice = Invoice
  { invoiceTitle          :: Text  -- ^ Product name.
  , invoiceDescription    :: Text  -- ^ Product description.
  , invoiceStartParameter :: Text  -- ^ Unique bot deep-linking parameter that can be used to generate this invoice.
  , invoiceCurrency       :: Text  -- ^ Three-letter ISO 4217 currency code.
  , invoiceTotalAmount    :: Int -- ^ Total price in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  }
  deriving (Generic, Show)

deriveJSON' ''Invoice
