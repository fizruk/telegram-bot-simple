{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Game where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Animation
import Telegram.Bot.API.Types.MessageEntity
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Game'

-- | This object represents a game. Use BotFather to create and edit games, their short names will act as unique identifiers.
data Game = Game
  { gameTitle        :: Text                  -- ^ Title of the game.
  , gameDescription  :: Text                  -- ^ Description of the game.
  , gamePhoto        :: [PhotoSize]           -- ^ Photo that will be displayed in the game message in chats.
  , gameText         :: Maybe Text            -- ^ Brief description of the game or high scores included in the game message. Can be automatically edited to include current high scores for the game when the bot calls setGameScore, or manually edited using editMessageText. 0-4096 characters.
  , gameTextEntities :: Maybe [MessageEntity] -- ^ Special entities that appear in text, such as usernames, URLs, bot commands, etc.
  , gameAnimation    :: Maybe Animation       -- ^ Animation that will be displayed in the game message in chats. Upload via @BotFather@.
  }
  deriving (Generic, Show)

instance ToJSON   Game where toJSON = gtoJSON
instance FromJSON Game where parseJSON = gparseJSON
