{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.SharedUser where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'SharedUser'

-- | This object contains information about a user that was shared with the bot using a 'KeyboardButtonRequestUsers' button.
data SharedUser = SharedUser
  { sharedUserUserId :: UserId -- ^ Identifier of the shared user. This number may have more than 32 significant bits and some programming languages may have difficulty\/silent defects in interpreting it. But it has at most 52 significant bits, so 64-bit integers or double-precision float types are safe for storing these identifiers. The bot may not have access to the user and could be unable to use this identifier, unless the user is already known to the bot by some other means.
  , sharedUserFirstName :: Maybe Text -- ^ First name of the user, if the name was requested by the bot.
  , sharedUserLastName :: Maybe Text -- ^ Last name of the user, if the name was requested by the bot.
  , sharedUserUsername :: Maybe Text -- ^ Username of the user, if the username was requested by the bot.
  , sharedUserPhoto :: Maybe [PhotoSize] -- ^ Available sizes of the chat photo, if the photo was requested by the bot.3
  }
  deriving (Generic, Show)

instance ToJSON   SharedUser where toJSON = gtoJSON
instance FromJSON SharedUser where parseJSON = gparseJSON
