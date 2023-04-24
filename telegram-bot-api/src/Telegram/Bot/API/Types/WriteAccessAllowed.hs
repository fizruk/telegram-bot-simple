{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.WriteAccessAllowed where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'WriteAccessAllowed'

-- | This object represents a service message about a user allowing a bot added to the attachment menu to write messages. Currently holds no information.
newtype WriteAccessAllowed = WriteAccessAllowed
  { writeAccessAllowedWebAppName :: Maybe Text -- ^ Name of the Web App which was launched from a link.
  }
  deriving (Generic, Show)

instance ToJSON   WriteAccessAllowed where toJSON = gtoJSON
instance FromJSON WriteAccessAllowed where parseJSON = gparseJSON
