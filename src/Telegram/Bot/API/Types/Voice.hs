{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.Voice where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Internal.Utils

-- ** 'Voice'

-- | This object represents a voice note.
data Voice = Voice
  { voiceFileId   :: FileId -- ^ Unique identifier for this file.
  , voiceFileUniqueId :: FileId -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , voiceDuration :: Seconds -- ^ Duration of the audio in seconds as defined by sender.
  , voiceMimeType :: Maybe Text -- ^ MIME type of the file as defined by sender.
  , voiceFileSize :: Maybe Integer -- ^ File size in bytes.
  }
  deriving (Generic, Show)

deriveJSON' ''Voice
