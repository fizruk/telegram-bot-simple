{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.CallbackGame where

import Data.Aeson (FromJSON (..), ToJSON (..), Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'CallbackGame'

-- | A placeholder, currently holds no information. Use BotFather to set up your game.
newtype CallbackGame = CallbackGame Object
  deriving (Generic, Show)

instance ToJSON   CallbackGame where toJSON = gtoJSON
instance FromJSON CallbackGame where parseJSON = gparseJSON
