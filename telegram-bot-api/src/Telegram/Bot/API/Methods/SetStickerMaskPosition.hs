{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetStickerMaskPosition where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH
import Telegram.Bot.API.Types

-- ** 'setStickerMaskPosition'

-- | Request parameters for 'setStickerMaskPosition'.
data SetStickerMaskPositionRequest = SetStickerMaskPositionRequest
  { setStickerMaskPositionSticker      :: Text -- ^ File identifier of the sticker
  , setStickerMaskPositionMaskPosition :: Maybe [MaskPosition] -- ^ A JSON-serialized object with the position where the mask should be placed on faces. Omit the parameter to remove the mask position.
  }
  deriving Generic

instance ToJSON   SetStickerMaskPositionRequest where toJSON = gtoJSON
instance FromJSON SetStickerMaskPositionRequest where parseJSON = gparseJSON

type SetStickerMaskPosition = "setStickerMaskPosition"
  :> ReqBody '[JSON] SetStickerMaskPositionRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change the mask position of a mask sticker.
--   The sticker must belong to a sticker set that was created by the bot.
--   Returns 'True' on success.
setStickerMaskPosition :: SetStickerMaskPositionRequest -> ClientM (Response Bool)
setStickerMaskPosition = client (Proxy @SetStickerMaskPosition)

makeDefault ''SetStickerMaskPositionRequest
