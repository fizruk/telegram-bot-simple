{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.VideoNote where

import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'VideoNote'

-- | This object represents a video message (available in Telegram apps as of v.4.0).
data VideoNote = VideoNote
  { videoNoteFileId   :: FileId -- ^ Unique identifier for this file.
  , videoNoteFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , videoNoteLength   :: Int -- ^ Video width and height as defined by sender.
  , videoNoteDuration :: Seconds -- ^ Duration of the video in seconds as defined by sender.
  , videoNoteThumb    :: Maybe PhotoSize -- ^ Video thumbnail.
  , videoNoteFileSize :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

deriveJSON' ''VideoNote
