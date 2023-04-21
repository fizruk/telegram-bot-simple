{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Audio where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Audio'

-- | This object represents an audio file to be treated as music by the Telegram clients.
data Audio = Audio
  { audioFileId    :: FileId -- ^ Unique identifier for this file.
  , audioFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , audioDuration  :: Seconds -- ^ Duration of the audio in seconds as defined by sender.
  , audioPerformer :: Maybe Text -- ^ Performer of the audio as defined by sender or by audio tags.
  , audioTitle     :: Maybe Text -- ^ Title of the audio as defined by sender or by audio tags.
  , audioFileName  :: Maybe Text -- ^ Original filename as defined by sender.
  , audioMimeType  :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , audioFileSize  :: Maybe Integer -- ^ File size in bytes.
  , audioThumbnail :: Maybe PhotoSize -- ^ Thumbnail of the album cover to which the music file belongs.
  }
  deriving (Generic, Show)

instance ToJSON   Audio where toJSON = gtoJSON
instance FromJSON Audio where parseJSON = gparseJSON
