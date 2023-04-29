{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.BotName where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Telegram.Bot.API.Internal.Utils

-- ** 'BotName'

-- | This object represents the bot's name.
newtype BotName = BotName
  { botNameName :: Text -- ^ The bot's name.
  }
  deriving (Generic, Show)

instance ToJSON   BotName where toJSON = gtoJSON
instance FromJSON BotName where parseJSON = gparseJSON
