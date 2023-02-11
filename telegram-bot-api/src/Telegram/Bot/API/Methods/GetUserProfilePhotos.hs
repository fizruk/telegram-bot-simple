{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.GetUserProfilePhotos where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Types

-- ** 'getUserProfilePhotos'

-- | Request parameters for 'getUserProfilePhotos'.
data GetUserProfilePhotosRequest = GetUserProfilePhotosRequest
  { getUserProfilePhotosUserId :: UserId -- ^ Unique identifier of the target user
  , getUserProfilePhotosOffset :: Maybe Int -- ^ Sequential number of the first photo to be returned. By default, all photos are returned.
  , getUserProfilePhotosLimit :: Maybe Int -- ^ Limits the number of photos to be retrieved. Values between 1-100 are accepted. Defaults to 100.
  }
  deriving Generic

instance ToJSON   GetUserProfilePhotosRequest where toJSON = gtoJSON
instance FromJSON GetUserProfilePhotosRequest where parseJSON = gparseJSON

type GetUserProfilePhotos = "getUserProfilePhotos"
  :> ReqBody '[JSON] GetUserProfilePhotosRequest
  :> Post '[JSON] (Response UserProfilePhotos)

-- | Use this method to get a list of profile pictures for a user.
--   Returns a UserProfilePhotos object.
getUserProfilePhotos :: GetUserProfilePhotosRequest ->  ClientM (Response UserProfilePhotos)
getUserProfilePhotos = client (Proxy @GetUserProfilePhotos)
