{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BotShortDescription where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Telegram.Bot.API.Internal.Utils

-- ** 'BotShortDescription'

-- | This object represents the bot's short description.
data BotShortDescription = BotShortDescription
  { botShortDescriptionShortDescription :: Text -- ^ The bot's short description.
  }
  deriving (Generic, Show)

instance ToJSON   BotShortDescription where toJSON = gtoJSON
instance FromJSON BotShortDescription where parseJSON = gparseJSON
