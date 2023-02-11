{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.ResponseParameters where

import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'ResponseParameters'

-- | Contains information about why a request was unsuccessful.
data ResponseParameters = ResponseParameters
  { responseParametersMigrateToChatId :: Maybe ChatId -- ^ The group has been migrated to a supergroup with the specified identifier. This number may be greater than 32 bits and some programming languages may have difficulty/silent defects in interpreting it. But it is smaller than 52 bits, so a signed 64 bit integer or double-precision float type are safe for storing this identifier.
  , responseParametersRetryAfter      :: Maybe Seconds -- ^ In case of exceeding flood control, the number of seconds left to wait before the request can be repeated
  }
  deriving (Show, Generic)

deriveJSON' ''ResponseParameters
