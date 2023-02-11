{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.EncryptedCredentials where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'EncryptedCredentials'

-- | Contains data required for decrypting and authenticating EncryptedPassportElement. See the Telegram Passport Documentation for a complete description of the data decryption and authentication processes.
data EncryptedCredentials = EncryptedCredentials
  { encryptedCredentialsData   :: Text -- ^ Base64-encoded encrypted JSON-serialized data with unique user's payload, data hashes and secrets required for EncryptedPassportElement decryption and authentication.
  , encryptedCredentialsHash   :: Text -- ^ Base64-encoded data hash for data authentication.
  , encryptedCredentialsSecret :: Text -- ^ Base64-encoded secret, encrypted with the bot's public RSA key, required for data decryption
  }
  deriving (Generic, Show)

instance ToJSON   EncryptedCredentials where toJSON = gtoJSON
instance FromJSON EncryptedCredentials where parseJSON = gparseJSON
