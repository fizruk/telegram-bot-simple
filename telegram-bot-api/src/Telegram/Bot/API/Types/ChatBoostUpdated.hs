{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatBoostUpdated where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.Types.Chat
import Telegram.Bot.API.Types.ChatBoost

-- ** 'ChatBoostUpdated'

-- | This object represents a boost added to a chat or changed.
data ChatBoostUpdated = ChatBoostUpdated
  { chatBoostUpdatedChat :: Chat -- ^ Chat which was boosted.
  , chatBoostUpdatedBoost :: ChatBoost -- ^ Infomation about the chat boost.
  }
  deriving (Generic, Show)

instance ToJSON   ChatBoostUpdated where toJSON = gtoJSON
instance FromJSON ChatBoostUpdated where parseJSON = gparseJSON
