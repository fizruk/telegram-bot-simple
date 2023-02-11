{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Contact where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Internal.Utils

-- ** 'Contact'

-- | This object represents a phone contact.
data Contact = Contact
  { contactPhoneNumber :: Text -- ^ Contact's phone number.
  , contactFirstName   :: Text -- ^ Contact's first name.
  , contactLastName    :: Maybe Text -- ^ Contact's last name.
  , contactUserId      :: Maybe UserId -- ^ Contact's user identifier in Telegram.
  , contactVcard       :: Maybe Text -- ^ Additional data about the contact in the form of a vCard.
  }
  deriving (Generic, Show)

instance ToJSON   Contact where toJSON = gtoJSON
instance FromJSON Contact where parseJSON = gparseJSON
