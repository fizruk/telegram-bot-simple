{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.PhotoSize where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'PhotoSize'

-- | This object represents one size of a photo or a file / sticker thumbnail.
data PhotoSize = PhotoSize
  { photoSizeFileId       :: FileId      -- ^ Unique identifier for this file.
  , photoSizeFileUniqueId :: FileId      -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , photoSizeWidth        :: Int       -- ^ Photo width
  , photoSizeHeight       :: Int       -- ^ Photo height
  , photoSizeFileSize     :: Maybe Int -- ^ File size
  }
  deriving (Generic, Show)

instance ToJSON   PhotoSize where toJSON = gtoJSON
instance FromJSON PhotoSize where parseJSON = gparseJSON
