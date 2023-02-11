{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.File where

import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common 
import Telegram.Bot.API.Internal.Utils

-- ** 'File'

-- | This object represents a file ready to be downloaded.
-- The file can be downloaded via the link @https://api.telegram.org/file/bot<token>/<file_path>@.
-- It is guaranteed that the link will be valid for at least 1 hour.
-- When the link expires, a new one can be requested by calling getFile.
data File = File
  { fileFileId       :: FileId      -- ^ Unique identifier for this file.
  , fileFileUniqueId :: FileId      -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , fileFileSize     :: Maybe Integer -- ^ File size in bytes, if known.
  , fileFilePath     :: Maybe Text  -- ^ File path. Use https://api.telegram.org/file/bot<token>/<file_path> to get the file.
  }
  deriving (Generic, Show)

deriveJSON' ''File
