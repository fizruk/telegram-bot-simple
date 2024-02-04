{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.UsersShared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'UsersShared'

-- | This object contains information about the users whose identifiers were shared with the bot using a 'KeyboardButtonRequestUsers' button.
data UsersShared = UsersShared
  { usersSharedRequestId :: RequestId -- ^ Identifier of the request.
  , usersSharedUserId :: [UserId] -- ^ Identifiers of the shared users. These numbers may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting them. But they have at most 52 significant bits, so 64-bit integers or double-precision float types are safe for storing these identifiers. The bot may not have access to the users and could be unable to use these identifiers, unless the users are already known to the bot by some other means.
  }
  deriving (Generic, Show)

instance ToJSON   UsersShared where toJSON = gtoJSON
instance FromJSON UsersShared where parseJSON = gparseJSON
