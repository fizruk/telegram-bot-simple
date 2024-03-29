{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ShippingQuery where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.ShippingAddress
import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'ShippingQuery'

-- | This object contains information about an incoming shipping query.
data ShippingQuery = ShippingQuery
  { shippingQueryId              :: Text            -- ^ Unique query identifier.
  , shippingQueryFrom            :: User            -- ^ User who sent the query.
  , shippingQueryInvoicePayload  :: Text            -- ^ Bot specified invoice payload.
  , shippingQueryShippingAddress :: ShippingAddress -- ^ User specified shipping address.
  }
  deriving (Generic, Show)

instance ToJSON   ShippingQuery where toJSON = gtoJSON
instance FromJSON ShippingQuery where parseJSON = gparseJSON
