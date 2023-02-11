{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.PassportFile where

import Data.Time.Clock.POSIX (POSIXTime)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Internal.Utils

-- ** 'PassportFile'

-- | This object represents a file uploaded to Telegram Passport. Currently all Telegram Passport files are in JPEG format when decrypted and don't exceed 10MB.
data PassportFile = PassportFile
  { passportFileFileId       :: FileId    -- ^ Identifier for this file, which can be used to download or reuse the file.
  , passportFileFileUniqueId :: FileId    -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , passportFileFileSize     :: Int     -- ^ File size in bytes.
  , passportFileFileDate     :: POSIXTime -- ^ Unix time when the file was uploaded.
  }
  deriving (Generic, Show)

deriveJSON' ''PassportFile
