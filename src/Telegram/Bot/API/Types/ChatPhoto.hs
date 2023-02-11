{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.ChatPhoto where

import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Internal.Utils

-- ** Chat photo

-- | Chat photo. Returned only in getChat.
data ChatPhoto = ChatPhoto
  { chatPhotoSmallFileId       :: FileId -- ^ Unique file identifier of small (160x160) chat photo. This file_id can be used only for photo download.
  , chatPhotoSmallFileUniqueId :: FileId -- ^ Unique file identifier of small (160x160) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , chatPhotoBigFileId         :: FileId -- ^ Unique file identifier of big (640x640) chat photo. This file_id can be used only for photo download.
  , chatPhotoBigFileUniqueId   :: FileId -- ^ Unique file identifier of big (640x640) chat photo, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  }
  deriving (Generic, Show)

instance ToJSON   ChatPhoto where toJSON = gtoJSON
instance FromJSON ChatPhoto where parseJSON = gparseJSON
