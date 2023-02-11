{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.LabeledPrice where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'LabeledPrice'

-- | This object represents a portion of the price for goods or services.
data LabeledPrice = LabelPrice
  { labeledPriceLabel  :: Text  -- ^ Portion label.
  , labeledPriceAmount :: Int -- ^ Price of the product in the smallest units of the currency (integer, not float/double). For example, for a price of US$ 1.45 pass amount = 145. See the exp parameter in currencies.json, it shows the number of digits past the decimal point for each currency (2 for the majority of currencies).
  }
  deriving (Generic, Show)

deriveJSON' ''LabeledPrice
