{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ShippingOption where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.LabeledPrice
import Telegram.Bot.API.Internal.Utils

-- ** 'ShippingOption'

-- | This object represents one shipping option.
data ShippingOption = ShippingOption
  { shippingOptionId    :: ShippingOptionId -- ^ Shipping option identifier.
  , shippingOptionTitle :: Text             -- ^ Option title.
  , shippingOptionPrice :: [LabeledPrice]   -- ^ List of price portions.
  }
  deriving (Generic, Show)

instance ToJSON   ShippingOption where toJSON = gtoJSON
instance FromJSON ShippingOption where parseJSON = gparseJSON
