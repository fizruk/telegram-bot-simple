{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.OrderInfo where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.ShippingAddress
import Telegram.Bot.API.Internal.Utils

-- ** 'OrderInfo'

-- | This object represents information about an order.
data OrderInfo = OrderInfo
  { orderInfoName            :: Maybe Text            -- ^ User name.
  , orderInfoPhoneNumber     :: Maybe Text            -- ^ User's phone number.
  , orderInfoEmail           :: Maybe Text            -- ^ User email.
  , orderInfoShippingAddress :: Maybe ShippingAddress -- ^ User shipping address.
  }
  deriving (Generic, Show)

deriveJSON' ''OrderInfo
