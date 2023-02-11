{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.BotCommand where

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

deriveJSON' ''BotCommand
