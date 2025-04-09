{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.UsersShared where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.SharedUser
import Telegram.Bot.API.Internal.Utils

-- ** 'UsersShared'

-- | This object contains information about the users whose identifiers were shared with the bot using a 'KeyboardButtonRequestUsers' button.
data UsersShared = UsersShared
  { usersSharedRequestId :: RequestId -- ^ Identifier of the request.
  , usersSharedUsers :: [SharedUser] -- ^ Information about users shared with the bot.
  }
  deriving (Generic, Show)

instance ToJSON   UsersShared where toJSON = gtoJSON
instance FromJSON UsersShared where parseJSON = gparseJSON
