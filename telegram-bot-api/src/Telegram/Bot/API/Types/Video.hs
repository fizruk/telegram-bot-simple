{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Video where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Video'

-- | This object represents a video file.
data Video = Video
  { videoFileId       :: FileId -- ^ Unique identifier for this file.
  , videoFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , videoWidth        :: Int -- ^ Video width as defined by sender.
  , videoHeight       :: Int -- ^ Video height as defined by sender.
  , videoDuration     :: Seconds -- ^ Duration of the video in seconds as defined by sender.
  , videoThumbnail    :: Maybe PhotoSize -- ^ Video thumbnail.
  , videoFileName     :: Maybe Text -- ^ Original filename as defined by sender.
  , videoMimeType     :: Maybe Text -- ^ Mime type of a file as defined by sender.
  , videoFileSize     :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

instance ToJSON   Video where toJSON = gtoJSON
instance FromJSON Video where parseJSON = gparseJSON
