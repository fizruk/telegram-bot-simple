{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Dice where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'Dice'

-- | This object represents an animated emoji that displays a random value.
data Dice = Dice
  { diceEmoji :: Text -- ^ Emoji on which the dice throw animation is based.
  , diceValue :: Int  -- ^ Value of the dice, 1-6 for “🎲”, “🎯” and “🎳” base emoji, 1-5 for “🏀” and “⚽” base emoji, 1-64 for “🎰” base emoji
  }
  deriving (Generic, Show)

deriveJSON' ''Dice
