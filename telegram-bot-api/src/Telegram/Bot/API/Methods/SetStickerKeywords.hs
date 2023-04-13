{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Telegram.Bot.API.Methods.SetStickerKeywords where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Proxy
import Data.Text
import GHC.Generics (Generic)
import Servant.API
import Servant.Client hiding (Response)

import Telegram.Bot.API.Internal.Utils
import Telegram.Bot.API.MakingRequests
import Telegram.Bot.API.Internal.TH

-- ** 'setStickerKeywords'

-- | Request parameters for 'setStickerKeywords'.
data SetStickerKeywordsRequest = SetStickerKeywordsRequest
  { setStickerKeywordsSticker :: Text -- ^ File identifier of the sticker
  , setStickerKeywordsKeywords :: Maybe [Text] -- ^ A JSON-serialized list of 0-20 search keywords for the sticker with total length of up to 64 characters
  }
  deriving Generic

instance ToJSON   SetStickerKeywordsRequest where toJSON = gtoJSON
instance FromJSON SetStickerKeywordsRequest where parseJSON = gparseJSON

type SetStickerKeywords = "setStickerKeywords"
  :> ReqBody '[JSON] SetStickerKeywordsRequest
  :> Post '[JSON] (Response Bool)

-- | Use this method to change search keywords
--   assigned to a regular or custom emoji sticker.
--   The sticker must belong to a sticker set created by the bot.
--   Returns 'True' on success.
setStickerKeywords :: SetStickerKeywordsRequest -> ClientM (Response Bool)
setStickerKeywords = client (Proxy @SetStickerKeywords)

makeDefault ''SetStickerKeywordsRequest
