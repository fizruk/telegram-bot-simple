{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BotCommand where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'BotCommand'

-- | This object represents a bot command.
data BotCommand = BotCommand
  { botCommandCommand :: Text -- ^ Text of the command; 1-32 characters. Can contain only lowercase English letters, digits and underscores.
  , botCommandDescription :: Text -- ^ Description of the command; 1-256 characters.
  }
  deriving (Generic, Show)

instance ToJSON   BotCommand where toJSON = gtoJSON
instance FromJSON BotCommand where parseJSON = gparseJSON
