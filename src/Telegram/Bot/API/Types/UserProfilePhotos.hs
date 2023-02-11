{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Telegram.Bot.API.Types.UserProfilePhotos where

import GHC.Generics (Generic)

import Telegram.Bot.API.Types.PhotoSize
import Telegram.Bot.API.Internal.Utils

-- ** 'UserProfilePhotos'

-- | This object represent a user's profile pictures.
data UserProfilePhotos = UserProfilePhotos
  { userProfilePhotosTotalCount :: Int -- ^ Total number of profile pictures the target user has
  , userProfilePhotosPhotos     :: [[PhotoSize]] -- ^ Requested profile pictures (in up to 4 sizes each)
  }
  deriving (Generic, Show)

deriveJSON' ''UserProfilePhotos
