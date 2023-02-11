{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.CallbackGame where

import Data.Aeson (Object)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'CallbackGame'

-- | A placeholder, currently holds no information. Use BotFather to set up your game.
newtype CallbackGame = CallbackGame Object
  deriving (Generic, Show)

deriveJSON' ''CallbackGame
