{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GiveawayCreated where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'GiveawayCreated'

-- | This object represents a service message about the creation of a scheduled giveaway. Currently holds no information.
data GiveawayCreated = GiveawayCreated
  { 
  }
  deriving (Generic, Show)

instance ToJSON   GiveawayCreated where toJSON = gtoJSON
instance FromJSON GiveawayCreated where parseJSON = gparseJSON
