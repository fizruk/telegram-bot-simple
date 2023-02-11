{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.UserShared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'UserShared'

-- | This object contains information about the user whose identifier was shared with the bot using a 'KeyboardButtonRequestUser' button.
data UserShared = UserShared
  { userSharedRequestId :: RequestId -- ^ Identifier of the request.
  , userSharedUserId :: UserId -- ^ Identifier of the shared user. This number may have more than 32 significant bits and some programming languages may have difficulty/silent defects in interpreting it. But it has at most 52 significant bits, so a 64-bit integer or double-precision float type are safe for storing this identifier. The bot may not have access to the user and could be unable to use this identifier, unless the user is already known to the bot by some other means.
  }
  deriving (Generic, Show)

instance ToJSON   UserShared where toJSON = gtoJSON
instance FromJSON UserShared where parseJSON = gparseJSON
