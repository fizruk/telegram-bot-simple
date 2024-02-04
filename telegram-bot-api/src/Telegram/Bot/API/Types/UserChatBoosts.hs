{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.UserChatBoosts where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.ChatBoost

-- ** 'UserChatBoosts'

-- | This object represents a list of boosts added to a chat by a user.
newtype UserChatBoosts = UserChatBoosts
  { userChatBoostsBosts :: [ChatBoost]
  }
  deriving (Generic, Show)

instance ToJSON   UserChatBoosts where toJSON = gtoJSON
instance FromJSON UserChatBoosts where parseJSON = gparseJSON
