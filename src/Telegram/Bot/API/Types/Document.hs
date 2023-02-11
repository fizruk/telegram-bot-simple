{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Document where 

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Document'

-- | This object represents a general file (as opposed to photos, voice messages and audio files).
data Document = Document
  { documentFileId   :: FileId -- ^ Unique file identifier.
  , documentFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , documentThumb    :: Maybe PhotoSize -- ^ Document thumbnail as defined by sender.
  , documentFileName :: Maybe Text -- ^ Original filename as defined by sender.
  , documentMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , documentFileSize :: Maybe Integer -- ^ File size in bytes. 
  }
  deriving (Generic, Show)

deriveJSON' ''Document
