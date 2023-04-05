{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetCustomEmojiStickerSetThumbnail where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setCustomEmojiStickerSetThumbnail'

data SetCustomEmojiStickerSetThumbnailRequest = SetCustomEmojiStickerSetThumbnailRequest
    { setCustomEmojiStickerSetThumbnailName          :: Text -- ^ Sticker set name
    , setCustomEmojiStickerSetThumbnailCustomEmojiId :: Maybe Text -- ^ Custom emoji identifier of a sticker from the sticker set; pass an empty string to drop the thumbnail and use the first sticker as the thumbnail.
    }
    deriving Generic

instance ToJSON   SetCustomEmojiStickerSetThumbnailRequest where toJSON = gtoJSON
instance FromJSON SetCustomEmojiStickerSetThumbnailRequest where parseJSON = gparseJSON

type SetCustomEmojiStickerSetThumbnail = "setCustomEmojiStickerSetThumbnail"
  :> ReqBody '[JSON] SetCustomEmojiStickerSetThumbnailRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to set the thumbnail of a custom emoji sticker set.
--   Returns 'True' on success.
setCustomEmojiStickerSetThumbnail :: SetCustomEmojiStickerSetThumbnailRequest -> ClientM (Response Bool)
setCustomEmojiStickerSetThumbnail = client (Proxy @SetCustomEmojiStickerSetThumbnail)

makeDefault ''SetCustomEmojiStickerSetThumbnailRequest