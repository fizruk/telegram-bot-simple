{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.GameHighScore where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.User
import Telegram.Bot.API.Internal.Utils

-- ** 'GameHighScore'

-- | This object represents one row of the high scores table for a game.
data GameHighScore = GameHighScore
  { gameHighScorePosition :: Int -- ^ Position in high score table for the game.
  , gameHighScoreUser     :: User  -- ^ User.
  , gameHighScoreScore    :: Int -- ^ Score.
  }
  deriving (Generic, Show)

instance ToJSON   GameHighScore where toJSON = gtoJSON
instance FromJSON GameHighScore where parseJSON = gparseJSON
