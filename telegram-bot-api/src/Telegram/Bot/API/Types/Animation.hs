{-# LANGUAGE DeriveGeneric #-}
module Telegram.Bot.API.Types.Animation where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)

import Telegram.Bot.API.Types.Common
import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'Animation'

-- | This object represents an animation file (GIF or H.264/MPEG-4 AVC video without sound).
data Animation = Animation
  { animationFileId       :: FileId          -- ^ Identifier for this file, which can be used to download or reuse the file.
  , animationFileUniqueId :: FileId          -- ^ Unique identifier for this file, which is supposed to be the same over time and for different bots. Can't be used to download or reuse the file.
  , animationWidth        :: Int           -- ^ Video width as defined by sender.
  , animationHeight       :: Int           -- ^ Video height as defined by sender.
  , animationDuration     :: Seconds         -- ^ Duration of the video in seconds as defined by sender.
  , animationThumb        :: Maybe PhotoSize -- ^ Animation thumbnail as defined by sender.
  , animationFileName     :: Maybe Text      -- ^ Original animation filename as defined by sender.
  , animationMimeType     :: Maybe Text      -- ^ MIME type of the file as defined by sender.
  , animationFileSize     :: Maybe Integer   -- ^ File size in bytes.
  }
  deriving (Generic, Show)

instance ToJSON   Animation where toJSON = gtoJSON
instance FromJSON Animation where parseJSON = gparseJSON
