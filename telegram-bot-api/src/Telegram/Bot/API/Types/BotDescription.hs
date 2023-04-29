{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BotDescription where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Telegram.Bot.API.Internal.Utils

-- ** 'BotDescription'

-- | This object represents the bot's description.
newtype BotDescription = BotDescription
  { botDescriptionDescription :: Text -- ^ The bot's description.
  }
  deriving (Generic, Show)

instance ToJSON   BotDescription where toJSON = gtoJSON
instance FromJSON BotDescription where parseJSON = gparseJSON
