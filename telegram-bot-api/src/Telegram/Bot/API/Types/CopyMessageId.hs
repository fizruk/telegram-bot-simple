{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.CopyMessageId where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'CopyMessageId'

-- | This object represents result of copyMessage request.
newtype CopyMessageId = CopyMessageId
  { copyMessageIdMessageId :: MessageId -- ^ the MessageId of the sent message.
  }
  deriving (Generic, Show)

instance ToJSON   CopyMessageId where toJSON = gtoJSON
instance FromJSON CopyMessageId where parseJSON = gparseJSON
