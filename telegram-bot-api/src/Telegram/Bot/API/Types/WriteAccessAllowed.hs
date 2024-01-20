{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.WriteAccessAllowed where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text
import GHC.Generics (Generic)

import Telegram.Bot.API.Internal.Utils

-- ** 'WriteAccessAllowed'

-- | This object represents a service message about a user allowing a bot added to the attachment menu to write messages. Currently holds no information.
data WriteAccessAllowed = WriteAccessAllowed
  { writeAccessAllowedFromRequest :: Maybe Bool -- ^ True, if the access was granted after the user accepted an explicit request from a Web App sent by the method 'requestWriteAccess'.
  , writeAccessAllowedWebAppName :: Maybe Text -- ^ Name of the Web App which was launched from a link.
  , writeAccessAllowedFromAttachmentMenu :: Maybe Bool -- ^ 'True', if the access was granted when the bot was added to the attachment or side menu.
  }
  deriving (Generic, Show)

instance ToJSON   WriteAccessAllowed where toJSON = gtoJSON
instance FromJSON WriteAccessAllowed where parseJSON = gparseJSON
