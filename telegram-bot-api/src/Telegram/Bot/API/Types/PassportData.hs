{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.PassportData where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.EncryptedCredentials
import Telegram.Bot.API.Types.EncryptedPassportElement
import Telegram.Bot.API.Internal.Utils

-- ** 'PassportData'

-- | Contains information about Telegram Passport data shared with the bot by the user.
data PassportData = PassportData
  { passportDataData        :: [EncryptedPassportElement] -- ^ Array with information about documents and other Telegram Passport elements that was shared with the bot.
  , passportDataCredentials :: EncryptedCredentials       -- ^ Encrypted credentials required to decrypt the data.
  }
  deriving (Generic, Show)

instance ToJSON   PassportData where toJSON = gtoJSON
instance FromJSON PassportData where parseJSON = gparseJSON
