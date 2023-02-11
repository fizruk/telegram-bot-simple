{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ShippingAddress where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'ShippingAddress'

-- | This object represents a shipping address.
data ShippingAddress = ShippingAddress
  { shippingAddressCountryCode :: Text -- ^ ISO 3166-1 alpha-2 country code.
  , shippingAddressState       :: Text -- ^ State, if applicable.
  , shippingAddressCity        :: Text -- ^ City.
  , shippingAddressStreetLine1 :: Text -- ^ First line for the address.
  , shippingAddressStreetLine2 :: Text -- ^ Second line for the address.
  , shippingAddressPostCode    :: Text -- ^ Address post code.
  }
  deriving (Generic, Show)

instance ToJSON   ShippingAddress where toJSON = gtoJSON
instance FromJSON ShippingAddress where parseJSON = gparseJSON
